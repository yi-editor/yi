{- 2>/dev/null

# To time the insertion of 1000 characters:
#
# ln -s Yi/Users/Cmcq.hs HackerMain.hs
# cabal build
# sh HackerMain.hs
#
# This uses the 'empty' tool (on Debian, apt-get install empty-expect).
#
# See http://code.google.com/p/yi-editor/issues/detail?id=265

empty -f -i in -o out -f dist/build/yi/yi time "$@" &&
    time cat out >/dev/null

exit 0
-}

import Yi
import Yi.Prelude
import Prelude (take)
import Yi.Keymap.Emacs (keymap)
import qualified Yi.UI.Common as Common
import qualified Yi.UI.Vty as Vty
import qualified Yi.UI.Pango as Pango
import Yi.Event (Event(Event),Key(KASCII,KEnter))
import Yi.Mode.Latex

import Control.Monad(unless)
import Control.Concurrent (forkIO)
import System.IO (readFile)
import System.Environment
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  let timing = "time" `elem` args
  unless timing (initDebug ".yi.dbg")
  let mystart = if "-fpango" `elem` args then Pango.start else Vty.start
  withArgs (if timing then [] else args) . yi $
    defaultEmacsConfig
    { defaultKm = keymap
    , startFrontEnd = (if timing then timeStart else id) mystart
    }

timeStart :: UIBoot -> UIBoot
timeStart start p1 ch actionCh p4 = do
  text <- take 1000 <$> readFile "doc/haskell08/haskell039-bernardy.tex"
  ui <- start p1 ch actionCh p4
  return ui
    { Common.main = do
        actionCh . (:[]) . makeAction . withBuffer0 $ setMode Yi.Mode.Latex.fastMode
        forkIO $ do
          forM_ text $ \x -> ch $ Yi.Event.Event (eventFromFile x) []
          ch $ Yi.Event.Event (Yi.Event.KASCII 'x') [MCtrl]
          ch $ Yi.Event.Event (Yi.Event.KASCII 'c') [MCtrl]
        Common.main ui
    }

eventFromFile :: Char -> Key
eventFromFile '\n' = KEnter
eventFromFile x = KASCII x
