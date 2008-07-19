{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}

module Yi.MiniBuffer 
 (
  spawnMinibufferE,
  withMinibufferFree, withMinibuffer, withMinibufferGen, withMinibufferFin, 
  noHint, noPossibilities, simpleComplete,
  matchingBufferNames
 ) where

import Control.Applicative
import Data.List (isInfixOf)
import Data.Typeable
import Data.Maybe
import Yi.Buffer
import Yi.Buffer.Region
import Yi.Buffer.HighLevel
import Yi.Config
import Yi.Core
import Yi.Editor
import Yi.History
import Yi.Completion (commonPrefix, infixMatch, prefixMatch, completeInList)
import Yi.Keymap
import Yi.Keymap.Keys
import qualified Yi.Editor as Editor
import qualified Yi.WindowSet as WS
import Control.Monad.Reader

-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: String -> KeymapEndo -> EditorM BufferRef
spawnMinibufferE prompt kmMod =
    do b <- stringToNewBuffer prompt (fromString "")
       fundamental <- asks fundamentalMode
       setBufferMode b fundamental {modeKeymap = kmMod}
       w <- newWindowE True b
       modifyWindows (WS.add w)
       return b

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once
-- a string @s@ is obtained, run @act s@. @completer@ can be used to complete
-- functions: it returns a list of possible matches.
withMinibuffer :: String -> (String -> YiM [String]) -> (String -> YiM ()) -> YiM ()
withMinibuffer prompt getPossibilities act = 
  withMinibufferGen "" giveHint prompt completer act
    where giveHint s = show . catMaybes . fmap (prefixMatch s) <$> getPossibilities s
          completer = simpleComplete getPossibilities

simpleComplete :: (String -> YiM [String]) -> String -> YiM String
simpleComplete getPossibilities s = do
              possibles <- getPossibilities s
              withEditor $ completeInList s (prefixMatch s) possibles

noHint :: String -> YiM String
noHint = const $ return ""

noPossibilities :: String -> YiM [ String ]
noPossibilities _s = return []

withMinibufferFree :: String
                                                  -> (String -> YiM ())
                                                  -> YiM ()
withMinibufferFree prompt = withMinibufferGen "" noHint prompt return

-- | @withMinibuffer proposal getHint prompt completer act@: open a minibuffer
-- with @prompt@, and initial content @proposal@. Once a string @s@ is obtained,
-- run @act s@. @completer@ can be used to complete inputs by returning an
-- incrementally better match, and getHint can give an immediate feedback to the
-- user on the current input.
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
        lineString <- withEditor $ do historyFinishGen prompt (withBuffer0 elemsB)
                                      lineString <- withBuffer0 elemsB
                                      closeMinibuffer
                                      switchToBufferE initialBuffer
                                      -- The above ensures that the action is performed on the buffer
                                      -- that originated the minibuffer.
                                      return lineString
        act lineString
      up   = historyMove prompt 1
      down = historyMove prompt (-1)

      rebindings = choice [oneOf [spec KEnter, ctrl $ char 'm'] >>! innerAction,
                           oneOf [spec KUp,    meta $ char 'p'] >>! up,
                           oneOf [spec KDown,  meta $ char 'n'] >>! down,
                           oneOf [spec KTab,   ctrl $ char 'i'] >>! completionFunction completer >>! showMatchings,
                           ctrl (char 'g')                     ?>>! closeMinibuffer]
  withEditor $ historyStartGen prompt
  msgEditor =<< getHint ""
  b <- withEditor $ spawnMinibufferE (prompt ++ " ") (\bindings -> rebindings <|| (bindings >> write showMatchings))
  withGivenBuffer b $ replaceBufferContent proposal


-- | Open a minibuffer, given a finite number of suggestions.
withMinibufferFin :: String -> [String] -> (String -> YiM ()) -> YiM ()
withMinibufferFin prompt posibilities act 
    = withMinibufferGen "" hinter prompt completer (act . best)
      where 
        -- The function for returning the hints provided to the user underneath
        -- the input, basically all those that currently match.
        hinter s = return $ show $ match s
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
        completer s = return $ case commonPrefix $ catMaybes $ fmap (infixMatch s) posibilities of
            "" -> s
            p -> p

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
    getPromptedValue :: String -> YiM a
    getPrompt :: a -> String           -- Parameter can be "undefined"
    doPrompt :: (a -> YiM ()) -> YiM ()
    doPrompt act = withMinibufferFree (getPrompt (undefined::a) ++ ": ") $ 
                     \string -> act =<< getPromptedValue string

instance Promptable String where
    getPromptedValue = return
    getPrompt _ = "String"

instance Promptable Int where
    getPromptedValue = return . read
    getPrompt _ = "Integer"

instance Promptable BufferRef where
    getPromptedValue n = withEditor $ getBufferWithName n
    getPrompt _ = "Buffer"
    doPrompt act = do 
      bufs <- matchingBufferNames ""
      withMinibufferFin "Buffer" bufs $ \n -> 
          do b <- withEditor $ getBufferWithName n
             act b

-- | Returns all the buffer names.
matchingBufferNames :: String -> YiM [String]
matchingBufferNames _s = withEditor $ do
  bs <- getBuffers
  return (map name bs)


-- TODO: be a bit more clever than 'Read r'
instance (YiAction a x, Promptable r, Typeable r) => YiAction (r -> a) x where
    makeAction f = YiA $ doPrompt (runAction . makeAction . f)
                   
