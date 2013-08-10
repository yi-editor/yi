module Yi.Keymap.Vim2.EventUtils
  ( stringToEvent
  , eventToString
  , parseEvents
  , stringToRepeatableAction
  , normalizeCount
  , splitCountedCommand
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (toUpper, isDigit)
import Data.List (break)
import qualified Data.Map as M
import Data.Tuple (swap)

import Yi.Event
import Yi.Keymap.Keys (char, ctrlCh, spec)
import Yi.Keymap.Vim2.Common

specMap :: M.Map EventString Key
specMap = M.fromList $ specList

invSpecMap :: M.Map Key EventString
invSpecMap = M.fromList $ fmap swap specList

specList :: [(String, Key)]
specList =
    [ ("<Esc>", KEsc)
    , ("<CR>", KEnter)
    , ("<BS>", KBS)
    , ("<Tab>", KTab)
    , ("<Down>", KDown)
    , ("<Up>", KUp)
    , ("<Left>", KLeft)
    , ("<Right>", KRight)
    , ("<PageUp>", KPageUp)
    , ("<PageDown>", KPageDown)
    , ("<Home>", KHome)
    , ("<End>", KEnd)
    , ("<Ins>", KIns)
    , ("<Del>", KDel)
    ]

stringToEvent :: String -> Event
stringToEvent ('<':'C':'-':c:'>':[]) = ctrlCh c
stringToEvent "<lt>" = char '<'
stringToEvent [c] = char c
stringToEvent ('<':'F':d:'>':[]) | isDigit d = spec (KFun $ read [d])
stringToEvent ('<':'F':'1':d:'>':[]) | isDigit d = spec (KFun $ 10 + read [d])
stringToEvent s =
    case M.lookup s specMap of
        Just k -> spec k
        Nothing -> error $ "Couldn't convert string <" ++ s ++ "> to event"

eventToString :: Event -> String
eventToString (Event (KASCII '<') []) = "<lt>"
eventToString (Event (KASCII c) []) = [c]
eventToString (Event (KASCII c) [MCtrl]) = ['<', 'C', '-', c, '>']
eventToString (Event (KASCII c) [MShift]) = [toUpper c]
eventToString (Event (KFun x) []) = "<F" ++ show x ++ ">"
eventToString e@(Event k []) =
    case M.lookup k invSpecMap of
        Just s -> s
        Nothing -> error $ "Couldn't convert event <" ++ show e ++ "> to string"
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
    where (count, command) = splitCountedCommand s

splitCountedCommand :: String -> (Int, String)
splitCountedCommand s = (count, commandString)
    where (countString, commandString) = break (not . isDigit) s
          count = case countString of
                   [] -> 1
                   _ -> read countString

-- 2d3w -> 6dw
-- 6dw -> 6dw
-- dw -> dw
normalizeCount :: String -> String
normalizeCount s = if null countedObject
                   then s
                   else show (operatorCount * objectCount) ++ operator ++ object
    where (operatorCount, rest1) = splitCountedCommand s
          (operator, countedObject) = break isDigit rest1
          (objectCount, object) = splitCountedCommand countedObject
