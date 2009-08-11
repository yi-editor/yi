{-

If Yi is called with parameter "time", it will insert 500 characters and exit.
Pango and Vty frontends are supported.

Pango build, test, profile:

    cabal build &&
        dist/build/yi/yi -fpango time +RTS -hr &&
        hp2ps -c yi.hp &&
        evince yi.ps

Vty timing using the "empty-expect" package on Debian:

    empty -f -i in -o out -f dist/build/yi/yi time "$@" &&
        time cat out >/dev/null

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
import Graphics.UI.Gtk hiding (on, Region, Window, Action, Point, Style)

nil :: IO a -> IO ()
nil = (() <$)

type IdleDo = [IO ()] -> IO ()
idleDoVty :: IdleDo
idleDoVty = nil . forkIO . sequence_

-- Run in the background enough that the display refereshes
idleDoPango :: IdleDo
idleDoPango = foldr idleDo1 (return ())
idleDo1 :: IO () -> IO () -> IO ()
idleDo1 a b = nil $ idleAdd (a >> b >> return False) priorityDefaultIdle

main :: IO ()
main = do
  args <- getArgs
  let timing = "time" `elem` args
      pango = "-fpango" `elem` args
  unless timing (initDebug ".yi.dbg")
  let mystart = if pango then Pango.start else Vty.start
  let idleDo  = if pango then idleDoPango else idleDoVty
  withArgs (if timing then [] else args) . yi $
    defaultEmacsConfig
    { defaultKm = keymap
    , startActions = []
    , startFrontEnd = (if timing then timeStart idleDo else id) mystart
    }

timeStart :: IdleDo -> UIBoot -> UIBoot
timeStart idleDo start p1 ch actionCh p4 = do
  text <- take 500 <$> readFile "doc/haskell08/haskell039-bernardy.tex"
  ui <- start p1 ch actionCh p4
  return ui
    { Common.main = do
        actionCh . (:[]) . makeAction . withBuffer0 $ setMode Yi.Mode.Latex.fastMode
        idleDo (fmap ch (actions text))
        Common.main ui
    }

actions :: String -> [Event]
actions text =
    fmap (\x -> Yi.Event.Event (eventFromFile x) []) text
    ++ [ Yi.Event.Event (Yi.Event.KASCII 'x') [MCtrl],
       Yi.Event.Event (Yi.Event.KASCII 'c') [MCtrl] ]

eventFromFile :: Char -> Key
eventFromFile '\n' = KEnter
eventFromFile x = KASCII x
