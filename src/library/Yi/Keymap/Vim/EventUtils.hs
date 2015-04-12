{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.EventUtils
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.EventUtils
  ( stringToEvent
  , eventToEventString
  , parseEvents
  , stringToRepeatableAction
  , normalizeCount
  , splitCountedCommand
  ) where

import           Data.Char            (isDigit, toUpper)
import           Data.List            (foldl')
import qualified Data.Map             as M (Map, fromList, lookup)
import           Data.Monoid          ((<>))
import qualified Data.Text            as T (break, cons, null, pack, singleton, snoc, span, unpack)
import           Data.Tuple           (swap)
import           Yi.Event
import           Yi.Keymap.Keys       (char, ctrl, meta, spec)
import           Yi.Keymap.Vim.Common (EventString (Ev), RepeatableAction (RepeatableAction))
import           Yi.String            (showT)

specMap :: M.Map EventString Key
specMap = M.fromList specList

invSpecMap :: M.Map Key EventString
invSpecMap = M.fromList $ fmap swap specList

specList :: [(EventString, Key)]
specList =
    [ (Ev "Esc", KEsc)
    , (Ev "CR", KEnter)
    , (Ev "BS", KBS)
    , (Ev "Tab", KTab)
    , (Ev "Down", KDown)
    , (Ev "Up", KUp)
    , (Ev "Left", KLeft)
    , (Ev "Right", KRight)
    , (Ev "PageUp", KPageUp)
    , (Ev "PageDown", KPageDown)
    , (Ev "Home", KHome)
    , (Ev "End", KEnd)
    , (Ev "Ins", KIns)
    , (Ev "Del", KDel)
    ]

stringToEvent :: String -> Event
stringToEvent "<" = error "Invalid event string \"<\""
stringToEvent "<C-@>" = (Event (KASCII ' ') [MCtrl])
stringToEvent s@('<':'C':'-':_) = stringToEvent' 3 s ctrl
stringToEvent s@('<':'M':'-':_) = stringToEvent' 3 s meta
stringToEvent s@('<':'a':'-':_) = stringToEvent' 3 s meta
stringToEvent "<lt>" = char '<'
stringToEvent [c] = char c
stringToEvent ('<':'F':d:'>':[]) | isDigit d = spec (KFun $ read [d])
stringToEvent ('<':'F':'1':d:'>':[]) | isDigit d = spec (KFun $ 10 + read [d])
stringToEvent s@('<':_) = stringToEvent' 1 s id
stringToEvent s = error ("Invalid event string " ++ show s)

stringToEvent' :: Int -> String -> (Event -> Event) -> Event
stringToEvent' toDrop inputString modifier =
  let analyzedString = drop toDrop inputString
  in case analyzedString of
    [c,'>'] -> modifier (char c)
    _ -> if last analyzedString /= '>'
         then error ("Invalid event string " ++ show inputString)
         else case M.lookup (Ev . T.pack $ init analyzedString) specMap of
           Just k -> modifier (Event k [])
           Nothing -> error $ "Couldn't convert string " ++ show inputString ++ " to event"

eventToEventString :: Event -> EventString
eventToEventString e = case e of
  Event (KASCII '<') []       -> Ev "<lt>"
  Event (KASCII ' ') [MCtrl]  -> Ev "<C-@>"
  Event (KASCII c)   []       -> Ev $ T.singleton c
  Event (KASCII c)   [MCtrl]  -> Ev $ mkMod MCtrl c
  Event (KASCII c)   [MMeta]  -> Ev $ mkMod MMeta c
  Event (KASCII c)   [MShift] -> Ev . T.singleton $ toUpper c
  Event (KFun x)     []       -> Ev $ "<F" <> showT x `T.snoc` '>'
  v@(Event      k    mods)    -> case M.lookup k invSpecMap of
    Just (Ev s) -> case mods of
      []      -> Ev $ '<' `T.cons` s `T.snoc` '>'
      [MCtrl] -> Ev $ "<C-" <> s `T.snoc` '>'
      [MMeta] -> Ev $ "<M-" <> s `T.snoc` '>'
      _ -> error $ "Couldn't convert event <" ++ show v
                   ++ "> to string, because of unknown modifiers"
    Nothing -> error $ "Couldn't convert event <" ++ show v ++ "> to string"

  where
    f MCtrl = 'C'
    f MMeta = 'M'
    f _     = '×'
    mkMod m c = '<' `T.cons` f m `T.cons` '-'
                `T.cons` c `T.cons` T.singleton '>'



parseEvents :: EventString -> [Event]
parseEvents (Ev x) = fst . foldl' go ([], []) $ T.unpack x
    where go (evs, s) '\n' = (evs, s)
          go (evs, []) '<' = (evs, "<")
          go (evs, []) c = (evs ++ [char c], [])
          go (evs, s) '>' = (evs ++ [stringToEvent (s ++ ">")], [])
          go (evs, s) c = (evs, s ++ [c])

stringToRepeatableAction :: EventString -> RepeatableAction
stringToRepeatableAction s = RepeatableAction count command
    where (count, command) = splitCountedCommand s

splitCountedCommand :: EventString -> (Int, EventString)
splitCountedCommand (Ev s) = (count, Ev commandString)
  where (countString, commandString) = T.span isDigit s
        count = case countString of
          "" -> 1
          x  -> read $ T.unpack x

-- 2d3w -> 6dw
-- 6dw -> 6dw
-- dw -> dw
normalizeCount :: EventString -> EventString
normalizeCount s =
  if T.null countedObject
  then s
  else Ev $ showT (operatorCount * objectCount) <> operator <> object
    where (operatorCount, Ev rest1) = splitCountedCommand s
          (operator, countedObject) = T.break isDigit rest1
          (objectCount, Ev object) = splitCountedCommand (Ev countedObject)
