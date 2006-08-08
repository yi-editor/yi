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
--

module Yi.Keymap.Joe (
    keymap
) where

import Control.Monad        ( when )
import Data.Maybe           ( isNothing )
import Yi.Editor            ( Action, withWindow_ )
import Yi.Yi hiding         ( keymap )
import Yi.CharMove
import Yi.Char
import Yi.MakeKeymap
import Yi.Buffer            ( moveTo, deleteN, insertN )

-- ---------------------------------------------------------------------

type JoeProc = KProc JoeState

data JoeState = JoeState {
    js_search_dir :: Direction,
    js_search_flags :: [SearchF],
    js_search_replace :: Maybe String
    }

initial_state :: JoeState
initial_state = JoeState {
    js_search_dir = GoRight,
    js_search_flags = [],
    js_search_replace = Nothing
    }

-- ---------------------------------------------------------------------

-- The Keymap

klist :: KList JoeState
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

keymap :: [Char] -> [Action]
keymap = makeKeymap klist initial_state

-- ---------------------------------------------------------------------

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
getFileE = do bufInfo <- bufInfoE
	      let fp = bufInfoFileName bufInfo
              return fp


-- TODO: This is slow, updating the screen on every char.
insertFileE :: String -> Action
insertFileE f = readFile f >>= mapM_ insertE


-- ---------------------------------------------------------------------

-- Helper routines

isCancel :: Char -> Bool
isCancel '\^G' = True
isCancel '\^C' = True
isCancel _     = False

isect :: Eq a => [a] -> [a] -> Bool
[] `isect` _ = False
(e:ee) `isect` l = e `elem` l || ee `isect` l

escape2rx :: String -> String
escape2rx []       = []
escape2rx ('^':cs) = '\\':'^':escape2rx cs
escape2rx (c:cs)   = '[':c:']':escape2rx cs


-- ---------------------------------------------------------------------

-- Query support

query_ :: String -> String -> (String -> JoeProc) -> JoeProc -> JoeProc
query_ prompt s cont failcont st cs =
    (msgE $ prompt ++ s):(loop cs)
    where
        q = \ss cs2 -> query_ prompt ss cont failcont st cs2
        loop (c:cs3)
            | isEnter c   = msgClrE:cmdlineUnFocusE:(cont s st cs3)
            | isCancel c  = msgClrE:cmdlineUnFocusE:(failcont st cs3)
            | isDel c     = q (init s) cs3
            | validChar c = q (s ++ [c]) cs3
            | otherwise   = loop cs3
        loop []           = []

query :: String -> String -> (String -> JoeProc) -> JoeProc -> JoeProc
query prompt s cont failcont st cs =
    cmdlineFocusE:(query_ prompt s cont failcont st cs)

simpleq :: String -> String -> (String -> Action) -> JoeProc -> JoeProc
simpleq prompt initial act cont st =
    query prompt initial (\s st_ cs -> (act s):(cont st_ cs)) cont st


queryKeys :: String -> [(String, JoeProc)] -> JoeProc -> JoeProc
queryKeys prompt ks failcont st cs_ =
    (msgE prompt):(loop cs_)
    where
        loop []                         = []
        loop (c:cs)
            | isEnter c || isCancel c   = msgClrE:(failcont st cs)
	    | otherwise                 = loop2 c ks cs
	loop2 _ []                      = loop
	loop2 c ((s, f):_) | c `elem` s = (msgClrE:) . (f st)
	loop2 c (_:ss)                  = loop2 c ss


-- ---------------------------------------------------------------------

-- Some queries

queryNewE, querySaveE, queryGotoLineE, queryInsertFileE,
    queryBufW, querySearchRepE, nextSearchRepE :: JoeProc -> JoeProc


queryNewE = simpleq "File name: " [] fnewE
queryGotoLineE = simpleq "Line number: " [] (gotoLnE . read)
queryInsertFileE = simpleq "File name: " [] insertFileE
queryBufW = simpleq "Buffer: " [] unimplementedQ
querySaveE cont st _ = return $
    getFileE >>= \f -> metaM $ simpleq "File name: " f fwriteToE cont st


-- ---------------------------------------------------------------------

-- Search queries

queryReplace :: SearchMatch
             -> String
             -> IO (Maybe SearchMatch)
             -> JoeProc
             -> JoeProc
queryReplace m s sfn cont =
    queryKeys prompt opts cont
    where
        prompt="Replace? (Y)es (N)o (R)est? "
	opts=[("yY", repl m False), ("rR", repl m True), ("nN", skip m)]
	
	skip (_, j) st _ = return $ do
	    res <- do_next j
            case res of
                Nothing -> metaM (cont st)
                Just p -> metaM $ queryReplace p s sfn cont st
	
	repl mm_ rest_ st_ cs_ = return $ repl_ mm_ rest_ st_ cs_
	   where
	       repl_ mm@(i, _) rest st cs = do
	           do_replace mm s
                   res <- do_next (i+length s)
                   case (res, rest) of
                       (Nothing, _)    -> metaM (cont st)
                       (Just p, True)  -> repl_ p rest st cs
                       (Just p, False) -> metaM $ queryReplace p s sfn cont st

	do_replace (i, j) ss = withWindow_ $ \w b -> do
	    moveTo b i
            deleteN b (j-i)
            insertN b ss
            return w

        do_next j = do
            op <- getPointE
	    gotoPointE (j-1) -- Don't replace within replacement
	                     -- TODO: backwards search
	    res <- sfn
	    when (isNothing res) (gotoPointE op)
	    return res
	
doSearch :: SearchExp -> JoeProc -> JoeState -> Action
doSearch srchexp cont st = do
    res <- sfn
    case res of
        Nothing -> errorE "Not found." >> metaM (cont st)
	Just p -> case js_search_replace st of
            Just rep -> metaM $ queryReplace p rep (sfn) cont st
            Nothing -> metaM $ cont st
    where
        sfn = do
            op <- getPointE
            res <- searchDoE srchexp (js_search_dir st)
	    case res of
                Just (Left _) -> gotoPointE op >> return Nothing
                Just (Right p) -> return (Just p)
		Nothing -> return Nothing


mksearch :: JoeProc -> String -> String -> Maybe String -> JoeProc
mksearch cont s flags repl st _ = return $ do
    srchexp <- searchInitE searchrx (js_search_flags newst)
    doSearch srchexp cont newst
    where
        ignore   = if isect "iI" flags then [IgnoreCase] else []
        dir      = if isect "bB" flags then GoLeft else GoRight
        searchrx = if isect "xX" flags then s else escape2rx s
        newst = st{
            js_search_dir = dir,
            js_search_flags = ignore,
            js_search_replace = repl
            }

querySearchRepE cont =
    query "Search term: " [] qflags cont
    where
        flagprompt = "(I)gnore, (R)eplace, (B)ackward Reg.E(x)p? "
        qflags s = query flagprompt [] (qreplace s) cont
	qreplace s flags | isect "rR" flags =
	    query "Replace with: " [] (mksearch cont s flags . Just) cont
	qreplace s flags =
	    mksearch cont s flags Nothing

nextSearchRepE cont st _ = return $
    getRegexE >>= \e -> case e of
        Nothing -> metaM $ querySearchRepE cont st
        Just se -> doSearch se cont st

unimplementedQ :: String -> Action
unimplementedQ _ = nopE
