module Yi.Event 
    (
     Event(..),
     Key(..), Modifier(..),

     -- * Key codes
     eventToChar,
     keyBreak, keyDown, keyUp, keyLeft, keyRight, keyHome,
     keyBackspace, keyDL, keyIL, keyDC, keyIC, keyEIC, keyClear,
     keyEOS, keyEOL, keySF, keySR, keyNPage, keyPPage, keySTab,
     keyCTab, keyCATab, keyEnter, keySReset, keyReset, keyPrint,
     keyLL, keyA1, keyA3, keyB2, keyC1, keyC3, keyBTab, keyBeg,
     keyCancel, keyClose, keyCommand, keyCopy, keyCreate, keyEnd,
     keyExit, keyFind, keyHelp, keyMark, keyMessage, keyMove, keyNext,
     keyOpen, keyOptions, keyPrevious, keyRedo, keyReference, keyRefresh,
     keyReplace, keyRestart, keyResume, keySave, keySBeg, keySCancel,
     keySCommand, keySCopy, keySCreate, keySDC, keySDL, keySelect, keySEnd,
     keySEOL, keySExit, keySFind, keySHelp, keySHome, keySIC, keySLeft,
     keySMessage, keySMove, keySNext, keySOptions, keySPrevious, keySPrint,
     keySRedo, keySReplace, keySRight, keySRsume, keySSave, keySSuspend,
     keySUndo, keySuspend, keyUndo,
    ) where

import Data.Bits
import Data.Char (chr,ord)
import Yi.Debug

data Modifier = MShift | MCtrl | MMeta
                deriving (Show,Eq,Ord)

data Key = KEsc | KFun Int | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter deriving (Eq,Show,Ord)

data Event = Event Key [Modifier] deriving (Show,Eq,Ord)



-- | Map an event to a char. This should be gotten rid of, eventually.

eventToChar :: Event -> Char
eventToChar (Event KBS _) = keyBackspace
eventToChar (Event KIns _) = keyIC
eventToChar (Event KHome _) = keyHome
eventToChar (Event KDel _) = keyDC
eventToChar (Event KEnd _) = keyEnd
eventToChar (Event KUp _) = keyUp
eventToChar (Event KDown _) = keyDown
eventToChar (Event KPageUp _) = keyPPage
eventToChar (Event KPageDown _) = keyNPage
eventToChar (Event KLeft _) = keyLeft
eventToChar (Event KRight _) = keyRight
eventToChar (Event KEnter _) = '\n'
eventToChar (Event KEsc _) = '\ESC'

eventToChar (Event (KASCII c) mods) = (if MMeta `elem` mods then setMeta else id) $
                                      (if MCtrl `elem` mods then ctrlLowcase else id) $
                                      c

eventToChar ev = trace ("Got event " ++ show ev) keyOops

charToEvent :: Char -> Event
charToEvent c 
    | c == keyBackspace = Event KBS                      [] 
    | c == keyHome      = Event KHome                    []
    | c == keyEnd       = Event KEnd                     []
    | c == keyUp        = Event KUp                      []
    | c == keyDown      = Event KDown                    []
    | c == keyPPage     = Event KPageUp                  []
    | c == keyNPage     = Event KPageDown                []
    | c == keyLeft      = Event KLeft                    []
    | c == keyRight     = Event KRight                   []
    | c == '\n'         = Event KEnter                   []
    | c == '\ESC'       = Event KEsc                     []    
    | ord c < 32        = Event (KASCII (lowcaseCtrl c)) [MCtrl]
    | otherwise         = Event (KASCII c)               [MCtrl]


remapChar :: Char -> Char -> Char -> Char -> Char -> Char
remapChar a1 b1 a2 _ c
    | a1 <= c && c <= b1 = chr $ ord c - ord a1 + ord a2
    | otherwise          = c

ctrlLowcase :: Char -> Char
ctrlLowcase   = remapChar 'a'   'z'   '\^A' '\^Z'

lowcaseCtrl = remapChar '\^A' '\^Z' 'a'   'z'

-- set the meta bit, as if Mod1/Alt had been pressed
setMeta :: Char -> Char
setMeta c = chr (setBit (ord c) metaBit)

metaBit :: Int
metaBit = 7


-- | Some constants for easy symbolic manipulation.
-- See man getch(3ncurses) for semantics.

keyBreak :: Char
keyBreak        = chr (257)

keyDown :: Char
keyDown         = chr (258)

keyUp :: Char
keyUp           = chr (259)

keyLeft :: Char
keyLeft         = chr (260)

keyRight :: Char
keyRight        = chr (261)

keyHome :: Char
keyHome         = chr (262)

keyBackspace :: Char
keyBackspace    = chr (263)

keyDL :: Char
keyDL           = chr (328)

keyIL :: Char
keyIL           = chr (329)

keyDC :: Char
keyDC           = chr (330)

keyIC :: Char
keyIC           = chr (331)

keyEIC :: Char
keyEIC          = chr (332)

keyClear :: Char
keyClear        = chr (333)

keyEOS :: Char
keyEOS          = chr (334)

keyEOL :: Char
keyEOL          = chr (335)

keySF :: Char
keySF           = chr (336)

keySR :: Char
keySR           = chr (337)

keyNPage :: Char
keyNPage        = chr (338)

keyPPage :: Char
keyPPage        = chr (339)

keySTab :: Char
keySTab         = chr (340)

keyCTab :: Char
keyCTab         = chr (341)

keyCATab :: Char
keyCATab        = chr (342)

keyEnter :: Char
keyEnter        = chr (343)

keySReset :: Char
keySReset       = chr (344)

keyReset :: Char
keyReset        = chr (345)

keyPrint :: Char
keyPrint        = chr (346)

keyLL :: Char
keyLL           = chr (347)

keyA1 :: Char
keyA1           = chr (348)

keyA3 :: Char
keyA3           = chr (349)

keyB2 :: Char
keyB2           = chr (350)

keyC1 :: Char
keyC1           = chr (351)

keyC3 :: Char
keyC3           = chr (352)

keyBTab :: Char
keyBTab         = chr (353)

keyBeg :: Char
keyBeg          = chr (354)

keyCancel :: Char
keyCancel       = chr (355)

keyClose :: Char
keyClose        = chr (356)

keyCommand :: Char
keyCommand      = chr (357)

keyCopy :: Char
keyCopy         = chr (358)

keyCreate :: Char
keyCreate       = chr (359)

keyEnd :: Char
keyEnd          = chr (360)

keyExit :: Char
keyExit         = chr (361)

keyFind :: Char
keyFind         = chr (362)

keyHelp :: Char
keyHelp         = chr (363)

keyMark :: Char
keyMark         = chr (364)

keyMessage :: Char
keyMessage      = chr (365)

keyMove :: Char
keyMove         = chr (366)

keyNext :: Char
keyNext         = chr (367)

keyOpen :: Char
keyOpen         = chr (368)

keyOptions :: Char
keyOptions      = chr (369)

keyPrevious :: Char
keyPrevious     = chr (370)

keyRedo :: Char
keyRedo         = chr (371)

keyReference :: Char
keyReference    = chr (372)

keyRefresh :: Char
keyRefresh      = chr (373)

keyReplace :: Char
keyReplace      = chr (374)

keyRestart :: Char
keyRestart      = chr (375)

keyResume :: Char
keyResume       = chr (376)

keySave :: Char
keySave         = chr (377)

keySBeg :: Char
keySBeg         = chr (378)

keySCancel :: Char
keySCancel      = chr (379)

keySCommand :: Char
keySCommand     = chr (380)

keySCopy :: Char
keySCopy        = chr (381)

keySCreate :: Char
keySCreate      = chr (382)

keySDC :: Char
keySDC          = chr (383)

keySDL :: Char
keySDL          = chr (384)

keySelect :: Char
keySelect       = chr (385)

keySEnd :: Char
keySEnd         = chr (386)

keySEOL :: Char
keySEOL         = chr (387)

keySExit :: Char
keySExit        = chr (388)

keySFind :: Char
keySFind        = chr (389)

keySHelp :: Char
keySHelp        = chr (390)

keySHome :: Char
keySHome        = chr (391)

keySIC :: Char
keySIC          = chr (392)

keySLeft :: Char
keySLeft        = chr (393)

keySMessage :: Char
keySMessage     = chr (394)

keySMove :: Char
keySMove        = chr (395)

keySNext :: Char
keySNext        = chr (396)

keySOptions :: Char
keySOptions     = chr (397)

keySPrevious :: Char
keySPrevious    = chr (398)

keySPrint :: Char
keySPrint       = chr (399)

keySRedo :: Char
keySRedo        = chr (400)

keySReplace :: Char
keySReplace     = chr (401)

keySRight :: Char
keySRight       = chr (402)

keySRsume :: Char
keySRsume       = chr (403)

keySSave :: Char
keySSave        = chr (404)

keySSuspend :: Char
keySSuspend     = chr (405)

keySUndo :: Char
keySUndo        = chr (406)

keySuspend :: Char
keySuspend      = chr (407)

keyUndo :: Char
keyUndo         = chr (408)

keyOops :: Char
keyOops = chr 0

