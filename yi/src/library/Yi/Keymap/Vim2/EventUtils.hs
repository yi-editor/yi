module Yi.Keymap.Vim2.EventUtils
  ( stringToEvent
  , eventToString
  , parseEvents
  , stringToRepeatableAction
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (toUpper, isDigit)
import Data.List (break)

import Yi.Event
import Yi.Keymap.Keys (char, ctrlCh, spec)
import Yi.Keymap.Vim2.Common

stringToEvent :: String -> Event
stringToEvent ('<':'C':'-':c:'>':[]) = ctrlCh c
stringToEvent "<Esc>" = spec KEsc
stringToEvent "<CR>" = spec KEnter
stringToEvent "<lt>" = char '<'
stringToEvent (c:[]) = char c
stringToEvent s = error $ "Couldn't convert string <" ++ s ++ "> to event"

eventToString :: Event -> String
eventToString (Event (KASCII '<') []) = "<lt>"
eventToString (Event (KASCII c) []) = [c]
eventToString (Event (KASCII c) [MCtrl]) = ['<', 'C', '-', c, '>']
eventToString (Event (KASCII c) [MShift]) = [toUpper c]
eventToString e = error $ "Couldn't convert event <" ++ show e ++ "> to string"

parseEvents :: String -> [Event]
parseEvents = fst . foldl' go ([], [])
    where go (evs, s) '\n' = (evs, s)
          go (evs, []) '<' = (evs, "<")
          go (evs, []) c = (evs ++ [char c], [])
          go (evs, s) '>' = (evs ++ [stringToEvent (s ++ ">")], [])
          go (evs, s) c = (evs, s ++ [c])

stringToRepeatableAction :: String -> RepeatableAction
stringToRepeatableAction s = RepeatableAction count command
    where (countString, command) = break (not . isDigit) s
          count = case countString of
                   [] -> 1
                   _ -> read countString
