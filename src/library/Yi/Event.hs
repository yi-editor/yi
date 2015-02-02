module Yi.Event
    (
     Event(..), prettyEvent,
     Key(..), Modifier(..),

     -- * Key codes
     eventToChar
    ) where

import Data.Bits   (setBit)
import Data.Char   (chr, ord)
import Data.Monoid (mappend)

data Modifier = MShift | MCtrl | MMeta | MSuper | MHyper
                deriving (Show,Eq,Ord)

data Key = KEsc | KFun Int | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter | KTab deriving (Eq,Show,Ord)

data Event = Event Key [Modifier] deriving (Eq)

instance Ord Event where
    compare (Event k1 m1) (Event k2 m2) = compare m1 m2 `mappend` compare k1 k2
    -- so, all Ctrl+char, meta+char, etc. all form a continuous range

instance Show Event where
    show = prettyEvent

prettyEvent :: Event -> String
prettyEvent (Event k mods) =
           concatMap ((++ "-") . prettyModifier) mods ++ prettyKey k
  where prettyKey (KFun i) = 'F' : show i
        prettyKey (KASCII c) = [c]
        prettyKey key = tail $ show key
        prettyModifier m = [ show m !! 1]



-- | Map an Event to a Char. This is used in the emacs keymap for Ctrl-Q and vim keymap 'insertSpecialChar'
eventToChar :: Event -> Char
eventToChar (Event KEnter _) = '\CR'
eventToChar (Event KEsc _)   = '\ESC'
eventToChar (Event KBS _)    = '\127'
eventToChar (Event KTab _)   = '\t'

eventToChar (Event (KASCII c) mods) = (if MMeta `elem` mods then setMeta else id) $
                                      (if MCtrl `elem` mods then ctrlLowcase else id) c

eventToChar _ev = '?'



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



