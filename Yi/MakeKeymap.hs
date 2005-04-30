-- 
-- Copyright (c) 2005 Tuomo Valkonen
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
-- | Routines for building keymaps, including submaps, from binding lists.
--

module Yi.MakeKeymap (
    KME,
    KProc,
    KList,
    KListEnt,
    (&&>),
    (++>),
    makeKeymap
) where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )
import Yi.Char
import Data.FiniteMap
import Data.Maybe

-- ---------------------------------------------------------------------

data KME = KMEAction Action | KMESubmap KM | KMEMode (KProc -> KProc)
type KProc = [Char] -> [Action]
type KM = FiniteMap Char KME
type KMLookup = KM -> Char -> KME
type KListEnt = ([Char], KME)
type KList = [KListEnt]

-- | Bind a key combination to an action in a binding list.
(++>) :: [Char] -> Action -> KListEnt
s ++> a = (s, KMEAction a)

-- | Bind a key combination to a binding processor in a binding list. 
-- The binding processor should call evaluate the parameter continuation 
-- binding processor when finished.
(&&>) :: [Char] -> (KProc -> KProc) -> KListEnt
s &&> a = (s, KMEMode a)

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> KProc
makeKeymap kmap cs = actions
    where 
        kfm = buildKeymap emptyFM kmap
        kproc = getActions (kfm, lookup_dflt) kproc
        actions = kproc (map remapBS cs)

getActions :: (KM, KMLookup) -> KProc -> KProc
getActions (fm, lk) cont (c:cs) = 
    case lk fm c of 
        KMEAction a -> a:(cont cs)
        KMEMode m -> m cont cs
        KMESubmap sfm -> getActions (sfm, lookup_submap) cont cs
getActions _ _ [] = []

-- Key lookup from a FiniteMap for submaps: tries many alternatives,
-- with different upcase and control bits.
lookup_submap :: KMLookup
lookup_submap fm c = 
    fromMaybe (KMEAction undefE) $
        listToMaybe $ catMaybes $ map (\f -> lookupFM fm $ f c) alts
    where
        alts = [id, upcaseCtrl, upcaseLowcase, lowcaseCtrl, lowcaseUpcase,
                ctrlUpcase, ctrlLowcase]
        

-- Key lookup from a FiniteMap for top-level map: falls back to inserting
-- printable characters if mapping was not found.
lookup_dflt :: KMLookup
lookup_dflt fm c =
    fromMaybe fallback (lookupFM fm c)
    where
        fallback = KMEAction $ if validChar c then insertE c else undefE

-- Builds a keymap (FiniteMap) from a key binding list, also creating 
-- submaps from key sequences.
buildKeymap :: KM -> KList -> KM
buildKeymap fm_ l =
    foldl addKey fm_ l
    where
        addKey fm (c:[], a) = addToFM fm c a
        addKey fm (c:cs, a) = 
            addToFM fm c $ KMESubmap $ 
                case lookupFM fm c of
                    Nothing             -> addKey emptyFM (cs, a)
                    Just (KMESubmap sm) -> addKey sm (cs, a)
                    _                   -> error "Invalid keymap table"
        addKey _ ([], _) = error "Invalid keymap table"

undefE :: Action
undefE = errorE $ "Key sequence not defined."
