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
import Data.Maybe
import qualified Data.Map as M

-- ---------------------------------------------------------------------

data KME s = KMEAction Action
           | KMESubmap (KM s)
           | KMEMode (KProc s -> KProc s)
type KProc s = s -> [Char] -> [Action]
type KM s = M.Map Char (KME s)
type KMLookup s = (KM s) -> Char -> (KME s)
type KListEnt s = ([Char], KME s)
type KList s = [KListEnt s]

-- | Bind a key combination to an action in a binding list.
(++>) :: [Char] -> Action -> KListEnt s
s ++> a = (s, KMEAction a)

-- | Bind a key combination to a binding processor in a binding list.
-- The binding processor should call evaluate the parameter continuation
-- binding processor when finished.
(&&>) :: [Char] -> (KProc s -> KProc s) -> KListEnt s
s &&> a = (s, KMEMode a)

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList s -> KProc s
makeKeymap kmap st cs = actions
    where
        kfm = buildKeymap M.empty kmap
        kproc = getActions (kfm, lookup_dflt) kproc
        actions = kproc st (map remapBS cs)

getActions :: (KM s, KMLookup s) -> KProc s -> KProc s
getActions (fm, lk) cont s (c:cs) =
    case lk fm c of
        KMEAction a -> a:(cont s cs)
        KMEMode m -> m cont s cs
        KMESubmap sfm -> getActions (sfm, lookup_submap) cont s cs
getActions _ _ _ [] = []

-- Key lookup from a Yi.Map.Map for submaps: tries many alternatives,
-- with different upcase and control bits.
lookup_submap :: KMLookup s
lookup_submap fm c =
    fromMaybe (KMEAction undefE) $
        listToMaybe $ catMaybes $ map (\f -> M.lookup (f c) fm) alts
    where
        alts = [id, upcaseCtrl, upcaseLowcase, lowcaseCtrl, lowcaseUpcase,
                ctrlUpcase, ctrlLowcase]


-- Key lookup from a Yi.Map.Map for top-level map: falls back to inserting
-- printable characters if mapping was not found.
lookup_dflt :: KMLookup s
lookup_dflt fm c =
    fromMaybe fallback (M.lookup c fm)
    where
        fallback = KMEAction $ if validChar c then insertE c else undefE

-- Builds a keymap (Yi.Map.Map) from a key binding list, also creating
-- submaps from key sequences.
buildKeymap :: KM s -> KList s -> KM s
buildKeymap fm_ l =
    foldl addKey fm_ l
    where
        addKey fm (c:[], a) = M.insert c a fm
        addKey fm (c:cs, a) =
            flip (M.insert c) fm $ KMESubmap $
                case M.lookup c fm of
                    Nothing             -> addKey M.empty (cs, a)
                    Just (KMESubmap sm) -> addKey sm (cs, a)
                    _                   -> error "Invalid keymap table"
        addKey _ ([], _) = error "Invalid keymap table"

undefE :: Action
undefE = errorE $ "Key sequence not defined."
