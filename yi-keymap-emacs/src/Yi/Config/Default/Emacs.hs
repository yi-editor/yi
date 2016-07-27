{-# LANGUAGE OverloadedStrings #-}
module Yi.Config.Default.Emacs (configureEmacs) where

import           Lens.Micro.Platform ((%=), (.=), (.~))
import           Yi.Buffer.Misc    (identA, directoryContentA)
import           Yi.Config.Misc    (ScrollStyle (..))
import           Yi.Editor         (buffersA, newBufferE)
import           Yi.Event          (Modifier (..), Key (..), Event (..))
import           Yi.Interact       (mkAutomaton, anyEvent, write, (||>), event, P)
import           Yi.Keymap         (makeAction)
import           Yi.Keymap.Emacs   (keymap)
import           Yi.Keymap.Keys    (printableChar, spec)
import           Yi.Config.Lens
import           Yi.Config.Simple  (ConfigM)
import qualified Yi.Rope           as R
import           Yi.Types

import           Control.Monad       (forever, unless, void)
import qualified Data.Map            as M
import           Lens.Micro.Platform (use, (^.))

configureEmacs :: ConfigM ()
configureEmacs = do
  configUIA %= (configScrollStyleA .~ Just SnapToCenter)
  defaultKmA .= keymap
  startActionsA %= (makeAction openScratchBuffer :)
  configInputPreprocessA .= escToMeta
  configKillringAccumulateA .= True

-- | Input preprocessor: Transform Esc;Char into Meta-Char
-- Useful for emacs lovers ;)
escToMeta :: P Event Event
escToMeta = mkAutomaton $ forever $ (anyEvent >>= write) ||> do
    _ <- event (spec KEsc)
    c <- printableChar
    write (Event (KASCII c) [MMeta])

-- | Open an emacs-like scratch buffer if no file is open.
openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do
  fileBufOpen <- any isFileOrDir . M.elems <$> use buffersA
  unless fileBufOpen $
    void . newBufferE (MemBuffer "scratch") $ R.unlines
            [ "This buffer is for notes you don't want to save."
            , "If you want to create a file, open that file,"
            , "then enter the text in that file's own buffer."
            , ""
            ]
  where
    isFileOrDir :: FBuffer -> Bool
    isFileOrDir attrs = case attrs ^. identA of
      MemBuffer  _ -> attrs ^. directoryContentA
      FileBuffer _ -> True