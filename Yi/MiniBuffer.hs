{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}

 module Yi.MiniBuffer (
        spawnMinibufferE, withMinibuffer
) where

import Data.Typeable
import Yi.Buffer
import Yi.Buffer.Region
import Yi.Core
import Yi.Editor
import Yi.History
import Yi.Keymap
import Yi.Keymap.Emacs.Keys
import qualified Yi.Editor as Editor
import qualified Yi.WindowSet as WS
import Control.Monad.Reader

-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: String -> KeymapEndo -> YiM () -> YiM ()
spawnMinibufferE prompt kmMod initialAction =
    do b <- withEditor $ stringToNewBuffer prompt []
       fundamental <- asks (fundamentalMode . yiConfig)
       setBufferMode b fundamental {modeKeymap = kmMod}
       withEditor $ do
         w <- newWindowE True b
         modifyWindows (WS.add w)
       initialAction

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once a string @s@ is obtained, run @act s@. @completer@ can be used to complete functions.
withMinibuffer :: String -> (String -> YiM String) -> (String -> YiM ()) -> YiM ()
withMinibuffer prompt completer act = do
  initialBuffer <- withEditor getBuffer
  let innerAction :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = closeBufferAndWindowE
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
                    ("C-i", write (completionFunction completer)),
                    ("TAB", write (completionFunction completer)),
                    ("C-g", write closeMinibuffer)]
  withEditor $ historyStart
  spawnMinibufferE (prompt ++ " ") (rebind rebindings) (return ())

completionFunction :: (String -> YiM String) -> YiM ()
completionFunction f = do
  p <- withBuffer pointB
  text <- withBuffer $ readRegionB $ mkRegion 0 p
  compl <- f text
  -- it's important to do this before removing the text,
  -- so if the completion function raises an exception, we don't delete the buffer contents.
  withBuffer $ do moveTo 0
                  deleteN p
                  insertN compl

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
