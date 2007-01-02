module Yi.Vty 
    (
     Pic,    
     
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

    -- * Colours
    Pair(..), Color,
    color,              -- :: String -> Maybe Color
    colorToAttr,

    -- * Attributes
    Attr(..),
    attr0, setBold, setReverse,
    attrSet,
    attrPlus,           -- :: Attr -> Attr -> Attr

               module Graphics.Vty
              ) where

import Data.Char (chr,ord)
import Graphics.Vty
import Data.Bits

import Yi.Debug

type Pic = [[(Char,Int)]]



-- | Map an event to a char. This should be gotten rid of, eventually.
-- Do this to enable using Alex lexers.
-- OTOH, we should screw Alex and do proper typing.

eventToChar :: Event -> Char
eventToChar (EvKey KBS _) = keyBackspace
eventToChar (EvKey KHome _) = keyHome
eventToChar (EvKey KEnd _) = keyEnd
eventToChar (EvKey KUp _) = keyUp
eventToChar (EvKey KDown _) = keyDown
eventToChar (EvKey KPageUp _) = keyPPage
eventToChar (EvKey KPageDown _) = keyNPage
eventToChar (EvKey KLeft _) = keyLeft
eventToChar (EvKey KRight _) = keyRight
eventToChar (EvKey KEnter _) = '\n'

eventToChar (EvKey (KASCII c) mods) = (if MMeta `elem` mods then setMeta else id) $
                                      (if MCtrl `elem` mods then ctrlLowcase else id) $
                                      c

eventToChar ev = trace ("Got event " ++ show ev) keyOops


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

--
-- | Some constants for easy symbolic manipulation.

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


newtype Pair = Pair Int
    deriving Show

newtype Color = Color Int
    deriving Show

color :: String -> Maybe Color


color "black"    = Just $ Color (0)

color "red"      = Just $ Color (1)

color "green"    = Just $ Color (2)

color "yellow"   = Just $ Color (3)

color "blue"     = Just $ Color (4)

color "magenta"  = Just $ Color (5)

color "cyan"     = Just $ Color (6)

color "white"    = Just $ Color (7)

color _          = Just $ Color (0)    -- NB

newtype Attr = Attr { fromAttr :: Int }
    deriving Show

attr0   :: Attr
attr0   = Attr (0)

setBold :: Attr -> Bool -> Attr
setBold = attrSet (Attr 2097152)

setReverse :: Attr -> Bool -> Attr
setReverse = attrSet (Attr 262144)

-- | bitwise combination of attributes
attrSet :: Attr -> Attr -> Bool -> Attr
attrSet (Attr b) (Attr a) False = Attr (a .&. complement b)
attrSet (Attr b) (Attr a) True  = Attr (a .|.            b)

attrPlus :: Attr -> Attr -> Attr
attrPlus (Attr a) (Attr b) = Attr (a .|. b)

------------------------------------------------------------------------

colorToAttr :: Color -> Attr
colorToAttr (Color x) = Attr x


