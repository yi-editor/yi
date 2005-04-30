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

module Yi.Keymap.Joe (
    keymap
) where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )
import Yi.CharMove
import Yi.Char
import Yi.MakeKeymap

-- ---------------------------------------------------------------------

-- The Keymap

klist :: KList
klist=[
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
    "\BS"  ++> bdeleteE, 
    "\^J"  ++> killEolE,
    "\^[J" ++> killSolE,
    "\^Y"  ++> killLineE,
    "\^_"  ++> undoE,
    "\^^"  ++> redoE,
    "\^X"  ++> skipWordE,
    "\^Z"  ++> bskipWordE,
    "\^W"  ++> killWordE,
    "\^O"  ++> bkillWordE,
    "\^KR" &&> queryInsertFileE,
    -- Search
    "\^KF" &&> querySearchRepE,
    "\^L"  &&> nextSearchRepE,
    "\^KL" &&> queryGotoLineE,
    -- Buffers
    "\^KS" &&> queryBufW,
    "\^C"  ++> closeE,
    "\^KD" &&> querySaveE,
    -- Copy&paste
    --"\^KB" ++> setMarkE,
    --"\^KK" ++> copyE,
    --"\"KY" ++> cutE,
    --"\"KC" ++> pasteE,
    --"\"KW" &&> querySaveSelectionE,
    --"\"K/" &&> queryFilterSelectionE,
    
    -- Windows
    "\^KN" ++> nextBufW,
    "\^KP" ++> prevBufW,
    "\^KS" ++> splitE,
    "\^KO" ++> nextWinE,
    "\^C"  ++> closeE, -- Wrong, should close buffer
    
    -- Global
    "\^R"  ++> refreshE,
    "\^KX" ++> quitE,
    "\^KZ" ++> suspendE,
    "\^KE" &&> queryNewE
    ]

keymap :: KProc
keymap = makeKeymap klist

-- Extra actions

killEolE, killSolE, killLineE :: Action

killEolE = killE
killLineE = solE >> killE >> deleteE
killSolE = do
    p <- getPointE
    solE
    pn <- getPointE
    deleteNE (p-pn)

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
            | isEnter c   = msgClrE:cmdlineUnFocusE:(cont s cs3)
            | c=='\^G'    = msgClrE:cmdlineUnFocusE:(failcont cs3)
            | isDel c     = q (init s) cs3
            | validChar c = q (s ++ [c]) cs3
            | otherwise   = loop cs3
        loop []           = []

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
