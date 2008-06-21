module Yi.Event 
    (
     Event(..), prettyEvent,
     Key(..), Modifier(..),

     -- * Key codes
     eventToChar
    --, charToEvent,
    ) where

import Data.Bits
import Data.Char (chr,ord)
import Yi.Debug

data Modifier = MShift | MCtrl | MMeta
                deriving (Show,Eq,Ord)

data Key = KEsc | KFun Int | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter | KTab deriving (Eq,Show,Ord)

data Event = Event Key [Modifier] deriving (Eq,Ord)

instance Show Event where
    show = prettyEvent

prettyEvent :: Event -> String
prettyEvent (Event k mods) =
           concatMap ((++ "-") . prettyModifier) mods ++ prettyKey k
  where prettyKey (KFun i) = 'F' : show i
        prettyKey (KASCII c) = [c]
        prettyKey key = tail $ show key
        prettyModifier m = [show m !! 1]
      


-- | Map an Event to a Char. This should be gotten rid of, eventually.
-- (the vim keymap should handle event directly)
eventToChar :: Event -> Char
eventToChar (Event KEnter _) = '\n'
eventToChar (Event KEsc _) = '\ESC'

eventToChar (Event (KASCII c) mods) = (if MMeta `elem` mods then setMeta else id) $
                                      (if MCtrl `elem` mods then ctrlLowcase else id) $
                                      c

eventToChar ev = trace ("Got event " ++ show ev) '\0'



remapChar :: Char -> Char -> Char -> Char -> Char -> Char
remapChar a1 b1 a2 _ c
    | a1 <= c && c <= b1 = chr $ ord c - ord a1 + ord a2
    | otherwise          = c

ctrlLowcase :: Char -> Char
ctrlLowcase   = remapChar 'a'   'z'   '\^A' '\^Z'

-- set the meta bit, as if Mod1/Alt had been pressed
setMeta :: Char -> Char
setMeta c = chr (setBit (ord c) metaBit)

metaBit :: Int
metaBit = 7



