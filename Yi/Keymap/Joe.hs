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
    --"\^L"  &&> nextSearchRepE,
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

query :: String -> String -> (String -> KProc) -> KProc
query prompt initial cont cs =
    q initial cs
    where
        q s cs2 = msgE (prompt ++ s):
            case getch cs2 of
                ('\n':cs3)        -> msgClrE:cont s cs3
                ('\r':cs3)        -> msgClrE:cont s cs3
                (c:cs3) | isDel c -> q (init s) cs3
                (c:cs3)           -> q (s ++ [c]) cs3
                []                -> []
                
        getch = dropWhile (not . valid)
        valid c = valid_char c || isDel c || c == '\n' || c == '\n'

simpleq :: String -> String -> (String -> Action) -> KProc -> KProc
simpleq prompt initial act cont =
    query prompt initial $ \s cs -> (act s):(cont cs)

queryNewE, querySaveE, queryGotoLineE, queryInsertFileE,
    queryBufW, querySearchRepE :: KProc -> KProc

queryNewE = simpleq "File name: " [] fnewE
querySaveE = simpleq "File name: " [] unimplementedQ
queryGotoLineE = simpleq "Line number: " [] (gotoLnE . read)
queryInsertFileE = simpleq "File name: " [] unimplementedQ
queryBufW = simpleq "Buffer: " [] unimplementedQ
querySearchRepE = simpleq "Search term: " [] unimplementedQ

unimplementedQ :: String -> Action
unimplementedQ _ = nopE

--readMaybeCall :: Read a => (a -> Action) -> a -> (String -> Action)
--readMaybeCall f s =
--    case reads s of
       


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
    fromMaybe     (KMEAction undefE) $
        try_maybe (lookupFM fm $ lowcase_ctrl c) $
        try_maybe (lookupFM fm $ upcase_ctrl c) $
                  (lookupFM fm c)
    where
        try_maybe a b = maybe a Just b
        --try_maybe a Nothing = a
        --try_maybe _ b = b

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

upcase_ctrl, lowcase_ctrl :: Char -> Char
upcase_ctrl c
    | '\^A' <= c && c <= '\^Z' = chr $ ord c - ord '\^A' + ord 'A'
    | otherwise = c
lowcase_ctrl c
    | '\^A' <= c && c <= '\^Z' = chr $ ord c - ord '\^A' + ord 'a'
    | otherwise = c

remap_bs :: Char -> Char
remap_bs k | isDel k = '\BS'
           | otherwise = k

isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False

