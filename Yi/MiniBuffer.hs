{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}

module Yi.MiniBuffer (
        spawnMinibufferE, withMinibuffer, withMinibufferGen, withMinibufferFin, noHint
    ) where

import Data.List (isInfixOf)
import Data.Typeable
import Yi.Buffer
import Yi.Buffer.Region
import Yi.Buffer.HighLevel
import Yi.Core
import Yi.Editor
import Yi.History
import Yi.Completion ( commonPrefix )
import Yi.Keymap
import Yi.Keymap.Emacs.Keys
import qualified Yi.Editor as Editor
import qualified Yi.WindowSet as WS
import Control.Monad.Reader

-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: String -> KeymapEndo -> YiM BufferRef
spawnMinibufferE prompt kmMod =
    do b <- withEditor $ stringToNewBuffer prompt []
       fundamental <- asks (fundamentalMode . yiConfig)
       setBufferMode b fundamental {modeKeymap = kmMod}
       withEditor $ do
         w <- newWindowE True b
         modifyWindows (WS.add w)
       return b

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once a string @s@ is obtained, run @act s@. @completer@ can be used to complete functions.
withMinibuffer :: String -> (String -> YiM String) -> (String -> YiM ()) -> YiM ()
withMinibuffer = withMinibufferGen "" noHint

noHint :: String -> YiM String
noHint = const $ return ""

-- | @withMinibuffer proposal getHint prompt completer act@: open a minibuffer with @prompt@, and initial content @proposal@. Once a string @s@ is obtained, run @act s@. @completer@ can be used to complete inputs, and getHint can give an immediate feedback to the user on the current input.
withMinibufferGen :: String -> (String -> YiM String) -> 
                     String -> (String -> YiM String) -> (String -> YiM ()) -> YiM ()
withMinibufferGen proposal getHint prompt completer act = do
  initialBuffer <- withEditor getBuffer
  let innerAction :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = closeBufferAndWindowE
      showMatchings = msgEditor =<< getHint =<< withBuffer elemsB
      innerAction = do
        lineString <- withEditor $ do historyFinish
                                      lineString <- withBuffer0 elemsB
                                      closeMinibuffer
                                      switchToBufferE initialBuffer
                                      -- The above ensures that the action is performed on the buffer
                                      -- that originated the minibuffer.
                                      return lineString
        act lineString
      rebindings = [("RET", write innerAction),
                    ("C-m", write innerAction),
                    ("M-p", write historyUp),
                    ("M-n", write historyDown),
                    ("<up>", write historyUp),
                    ("<down>", write historyDown),
                    ("C-i", write (completionFunction completer) >> write showMatchings),
                    ("TAB", write (completionFunction completer) >> write showMatchings),
                    ("C-g", write closeMinibuffer)]
  withEditor historyStart
  msgEditor =<< getHint ""
  b <- spawnMinibufferE (prompt ++ " ") (\bindings -> rebind rebindings (bindings >> write showMatchings))
  withGivenBuffer b $ replaceBufferContent proposal


-- | Open a minibuffer, given a finite number of suggestions.
withMinibufferFin :: String -> [String] -> (String -> YiM ()) -> YiM ()
withMinibufferFin prompt posibilities act 
    = withMinibufferGen "" hinter prompt completer (act . best)
      where 
        -- The function for returning the hints provided to the user underneath
        -- the input, basically all those that currently match.
        hinter  = (\s -> return $ show $ match s)
        -- All those which currently match.
        match s = filter (s `isInfixOf`) posibilities
        -- The best match from the list of matches.
        best s = let ms = match s in
                 case ms of
                   [] -> s
                   (x:_) -> x
        -- We still want "TAB" to complete even though the user could just press
        -- return with an incomplete possibility. The reason is we may have for
        -- example two possibilities which share a long prefix and hence we wish
        -- to press tab to complete up to the point at which they differ.
        completer s = return . commonPrefix $ filter (isInfixOf s) posibilities

completionFunction :: (String -> YiM String) -> YiM ()
completionFunction f = do
  p <- withBuffer pointB
  let r = mkRegion 0 p
  text <- withBuffer $ readRegionB r
  compl <- f text
  -- it's important to do this before removing the text,
  -- so if the completion function raises an exception, we don't delete the buffer contents.
  withBuffer $ replaceRegionB r compl

class Promptable a where
    getPromptedValue :: String -> a
    getPrompt :: a -> String           -- Parameter can be "undefined"

instance Promptable String where
    getPromptedValue = id
    getPrompt _ = "String"

instance Promptable Int where
    getPromptedValue = read
    getPrompt _ = "Integer"

-- TODO: be a bit more clever than 'Read r'
instance (YiAction a x, Promptable r, Typeable r) => YiAction (r -> a) x where
    makeAction f = YiA $ withMinibuffer (getPrompt (undefined::r)) return $
                    \string ->  runAction $ makeAction $ f $ getPromptedValue string
