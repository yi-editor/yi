{-

If Yi is called with parameter "--type", it will insert 500 characters and exit.
With "--open", it will open a buffer, type, and kill the buffer, 50 times.
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
--import Yi.Mode.Latex
import Yi.Mode.Haskell

import Control.Concurrent (forkIO)
import System.IO (readFile)
import System.Environment
import Data.Char
import Graphics.UI.Gtk hiding (on, Region, Window, Action, Point, Style)
import Control.Exception
import qualified Data.List as L
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
  when ("--debug" `elem` args) (initDebug ".yi.dbg")
  let typing = "--type" `elem` args
      opening = "--open" `elem` args
      pango = "-fpango" `elem` args
      testing = typing || opening

  evs <- if typing then do typingEvs else openingEvs

  let mystart = if pango then Pango.start else Vty.start
  let idleDo  = if pango then idleDoPango else idleDoVty
  withArgs (if testing then [] else args) . yi $
    defaultEmacsConfig
    { defaultKm = keymap
    , startActions = []
    , startFrontEnd = (if testing then timeStart idleDo evs else id) mystart
    , configInputPreprocess = idAutomaton
    }

timeStart :: IdleDo -> [Event] -> UIBoot -> UIBoot
timeStart idleDo evs start p1 ch actionCh p4 = do
  ui <- start p1 ch actionCh p4
  evaluate (L.length evs)
  return ui
    { Common.main = do
        actionCh . (:[]) . makeAction . withCurrentBuffer $ setMode Yi.Mode.Haskell.fastMode
        idleDo (fmap ch evs)
        Common.main ui
    }

-- Test sequences
typingEvs, openingEvs :: IO [Event]

typingEvs = do
  --text <- take 500 <$> readFile "doc/haskell08/haskell039-bernardy.tex"
  text <- readFile "src/Yi/Buffer.hs"
  return (fmap (\x -> Yi.Event.Event (keyFromFile x) []) text ++ quit)

openingEvs = return (times 50 openAndKill ++ quit)

-- Event helpers
times :: Int -> [a] -> [a]
times n = concat . replicate n

ckey, key :: Char -> Event
ckey x = Event (Yi.Event.KASCII x) [MCtrl]
key x = Event (Yi.Event.KASCII x) []

ret :: Event
ret = Event KEnter []

quit, openAndKill, open, blah, killmessages, close, confirm :: [Event]
quit = [ckey 'x', ckey 'c']
openAndKill = open ++ blah ++ killmessages ++ close ++ confirm
open = [ckey 'x', ckey 'f', key 'z', ret]
blah = replicate 20 (key 'x')
-- clear undo information from the *messages* buffer
killmessages = [ckey 'x', key 'b', ret, ckey 'x', key 'k', ret]
close = [ckey 'x', key 'k', ret]
confirm = [key 'y']

keyFromFile :: Char -> Key
keyFromFile '\n' = KEnter
keyFromFile x = KASCII x
