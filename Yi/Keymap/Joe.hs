-- 
-- Copyright (c) 2004 Tuomo Valkonen
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

--
-- Joe-ish keymap for Yi. 

module Yi.Keymap.Joe (keymap) where

import Prelude hiding   ( any )

import Yi.Core
import Yi.Editor            ( Action )
import Yi.Curses            ( keyBackspace )

import Data.Maybe
--import Data.List            ( (\\) )
import Data.Char            ( isControl, ord, chr )
--import Control.Monad        ( liftM )
--import Control.Exception    ( ioErrors, catchJust )
import Data.FiniteMap

-- ---------------------------------------------------------------------

data KME = KMEAction Action | KMESubmap KM | KMEMode (KProc -> KProc)
type KProc = [Char] -> [Action]
type KM = FiniteMap Char KME
type KMLookup = KM -> Char -> KME
type KListEnt = ([Char], KME)
type KList = [KListEnt]

(++>) :: [Char] -> Action -> KListEnt
s ++> a = (s, KMEAction a)

(&&>) :: [Char] -> (KProc -> KProc) -> KListEnt
s &&> a = (s, KMEMode a)

-- The Keymap

kmap :: KList
kmap=[
    -- Editing and movement
    "\^KU" ++> topE,
    "\^KV" ++> botE,
    "\^A"  ++> solE,
    "\^E"  ++> eolE,
    "\^B"  ++> leftE,
    "\^F"  ++> rightE,
    "\^P"  ++> upE,
    "\^N"  ++> downE,
    "\^U"  ++> upScreenE,
    "\^V"  ++> downScreenE,
    "\^D"  ++> deleteE,
    "\BS"  ++> bsE, 
    "\^J"  ++> killEolE,
    "\^Y"  ++> killLineE,
    "\^_"  ++> undoE,
    "\^^"  ++> redoE,
    --"\^X"  ++> skipWordE,
    --"\^Z"  ++> bskipWordE,
    "\^KR"  &&> queryInsertFileE,
    -- Search
    "\^KF" &&> querySearchRepE,
    "\^L"  &&> nextSearchRepE,
    "\^KL" &&> queryGotoLineE,
    -- Buffers
    "\^KN" ++> nextBufW,
    "\^KP" ++> prevBufW,
    "\^KS" &&> queryBufW,
    "\^C"  ++> closeE,
    "\^KD" &&> querySaveE,
    -- Copy&paste
    --"\^KB" ++> setMarkE,
    --"\^KK" ++> copyE,
    --"\"KY" ++> cutE,
    --"\"KC" ++> pasteE,
    
    -- Global
    "\^KX" ++> quitE,
    "\^KE" &&> queryNewE
    ]

-- Extra actions

bsE, killEolE, killLineE, undefE :: Action

bsE = leftE >> deleteE
killEolE = killE
killLineE = solE >> killE
undefE = errorE $ "Key sequence not defined."

getFileE :: IO FilePath
getFileE = bufInfoE >>= \(fp, _, _, _, _, _) -> return fp

-- TODO: This is slow, updating the screen on every char.
insertFileE :: String -> Action
insertFileE f = readFile f >>= mapM_ insertE

query_ :: String -> String -> (String -> KProc) -> KProc -> KProc
query_ prompt s cont failcont cs =
    (msgE $ prompt ++ s):(loop cs)
    where
        q = \ss cs2 -> query_ prompt ss cont failcont cs2
        loop (c:cs3)
            | is_enter c   = msgClrE:cmdlineUnFocusE:(cont s cs3)
            | c=='\^G'     = msgClrE:cmdlineUnFocusE:(failcont cs3)
            | isDel c      = q (init s) cs3
            | valid_char c = q (s ++ [c]) cs3
            | otherwise    = loop cs3
        loop []            = []

query :: String -> String -> (String -> KProc) -> KProc -> KProc
query prompt s cont failcont cs =
    cmdlineFocusE:(query_ prompt s cont failcont cs)

simpleq :: String -> String -> (String -> Action) -> KProc -> KProc
simpleq prompt initial act cont =
    query prompt initial (\s cs -> (act s):(cont cs)) cont

queryNewE, querySaveE, queryGotoLineE, queryInsertFileE,
    queryBufW, querySearchRepE, nextSearchRepE :: KProc -> KProc


queryNewE = simpleq "File name: " [] fnewE
queryGotoLineE = simpleq "Line number: " [] (gotoLnE . read)
queryInsertFileE = simpleq "File name: " [] insertFileE
queryBufW = simpleq "Buffer: " [] unimplementedQ
querySaveE cont _ = return $
    getFileE >>= \f -> metaM $ simpleq "File name: " f fwriteToE cont

isect :: Eq a => [a] -> [a] -> Bool
[] `isect` _ = False
(e:ee) `isect` l = e `elem` l || ee `isect` l

mksearch :: String -> String -> Action
mksearch s flags = 
    searchE (Just s) [ignore] backward
    where
        ignore = if isect "iI" flags then IgnoreCase else Basic
        backward = if isect "bB" flags 
                       then \() -> Left () 
                       else \() -> Right ()
        
querySearchRepE cont cs = 
    query "Search term: " [] qflags cont cs
    where
        flagprompt = "[Not fully implemented] (I)gnore, (R)eplace, (B)ackward Reg.E(x)p? "
        qflags s cs2 = query flagprompt [] (doit s) cont cs2
        doit s flags cs3 = (mksearch s flags):(cont cs3)

nextSearchRepE cont _ = return $
    getRegexE >>= \e -> case e of
        Nothing -> metaM $ querySearchRepE cont
        Just _ -> metaM $ \cs -> (nopE):(cont cs)

unimplementedQ :: String -> Action
unimplementedQ _ = nopE


-- Keymap processing

-- | Top level. Lazily consume all the input, generating a list of
-- actions, which then need to be forced
--
-- NB . if there is a (bad) exception, we'll lose any new bindings.. iorefs?
--    . also, maybe we shouldn't refresh automatically?
--
keymap :: KProc
keymap cs = actions
    where 
        kfm = build_kmap emptyFM kmap
        kproc = get_actions (kfm, lookup_dflt) kproc
        actions = kproc (map remap_bs cs)

get_actions :: (KM, KMLookup) -> KProc -> KProc
get_actions (fm, lk) cont (c:cs) = 
    case lk fm c of 
        KMEAction a -> a:(cont cs)
        KMEMode m -> m cont cs
        KMESubmap sfm -> get_actions (sfm, lookup_submap) cont cs
get_actions _ _ [] = []

lookup_submap :: KMLookup
lookup_submap fm c = 
    fromMaybe (KMEAction undefE) $
        listToMaybe $ catMaybes $ map (\f -> lookupFM fm $ f c) alts
    where
        alts = [id, upcase_ctrl, upcase_lowcase, lowcase_ctrl, lowcase_upcase,
                ctrl_upcase, ctrl_lowcase]
        

lookup_dflt :: KMLookup
lookup_dflt fm c =
    fromMaybe fallback (lookupFM fm c)
    where
        fallback = KMEAction $ if valid_char c then insertE c else undefE

build_kmap :: KM -> KList -> KM
build_kmap fm_ l =
    foldl add_k fm_ l
    where
        add_k fm (c:[], a) = addToFM fm c a
        add_k fm (c:cs, a) = 
            addToFM fm c $ KMESubmap $ 
                case lookupFM fm c of
                    Nothing -> add_k emptyFM (cs, a)
                    Just (KMESubmap sm) -> add_k sm (cs, a)
                    _ -> error "Invalid keymap table"
        add_k _ ([], _) = error "Invalid keymap table"

-- Backspace remapping

valid_char :: Char -> Bool
valid_char '\n' = True
valid_char '\r' = True
valid_char c | isControl c = False
valid_char _ = True

remap_char :: Char -> Char -> Char -> Char -> Char -> Char
remap_char a1 b1 a2 _ c
    | a1 <= c && c <= b1 = chr $ ord c - ord a1 + ord a2
    | otherwise          = c

upcase_ctrl, lowcase_ctrl :: Char -> Char
upcase_lowcase, ctrl_lowcase :: Char -> Char
lowcase_upcase, ctrl_upcase :: Char -> Char
upcase_ctrl = remap_char '\^A' '\^Z' 'A' 'Z'
lowcase_ctrl = remap_char '\^A' '\^Z' 'a' 'z'
upcase_lowcase = remap_char 'a' 'z' 'A' 'Z'
ctrl_lowcase = remap_char 'a' 'z' '\^A' '\^Z'
lowcase_upcase = remap_char 'A' 'Z' 'a' 'z'
ctrl_upcase = remap_char 'A' 'Z' '\^A' '\^Z'

remap_bs :: Char -> Char
remap_bs k | isDel k = '\BS'
           | otherwise = k

isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False

is_enter :: Char -> Bool
is_enter '\n' = True
is_enter '\r' = True
is_enter _    = False
