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

import Control.Monad.State
import Yi.Yi
import Yi.CharMove
import Yi.Char

-- ---------------------------------------------------------------------

type JoeProc a = (Interact Char) a

type JoeMode = JoeProc ()

-- ---------------------------------------------------------------------

-- The Keymap

(++>) :: String -> Action -> JoeMode
s ++> a = s &&> write a

(&&>) :: String -> JoeMode -> JoeMode
s &&> p = mapM_ event s >> p

klist :: JoeMode
klist = choice [
    -- Editing and movement
    "\^K\^U" ++> topE,
    "\^K\^V" ++> botE,
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
    "\^K\^R" &&> queryInsertFileE,
    -- Search
    --"\^K\^F" &&> querySearchRepE,
    --"\^L"  &&> nextSearchRepE,
    "\^K\^L" &&> queryGotoLineE,
    -- Buffers
    "\^K\^S" &&> queryBufW,
    "\^C"  ++> closeE,
    "\^K\^D" &&> querySaveE,
    -- Copy&paste
    --"\^K^B" ++> setMarkE,
    --"\^K^K" ++> copyE,
    --"\"K^Y" ++> cutE,
    --"\"K^C" ++> pasteE,
    --"\"K^W" &&> querySaveSelectionE,
    --"\"K/" &&> queryFilterSelectionE,

    -- Windows
    "\^K\^N" ++> nextBufW,
    "\^K\^P" ++> prevBufW,
    "\^K\^S" ++> splitE,
    "\^K\^O" ++> nextWinE,
    "\^C"  ++> closeE, -- Wrong, should close buffer

    -- Global
    "\^R"  ++> refreshE,
    "\^K\^X" ++> quitE,
    "\^K\^Z" ++> suspendE,
    "\^K\^E" &&> queryNewE
    ]

keymap :: Keymap
keymap = runProc klist

runProc :: JoeMode -> Keymap
runProc = comap eventToChar

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

-- Commenting out to avoid compile warnings until fn is needed
-- getFileE :: EditorM FilePath
-- getFileE = do bufInfo <- bufInfoE
-- 	      let fp = bufInfoFileName bufInfo
--               return fp


insertFileE :: String -> Action
insertFileE f = lift (readFile f) >>= insertNE


-- ---------------------------------------------------------------------

-- Helper routines

isCancel :: Char -> Bool
isCancel '\^G' = True
isCancel '\^C' = True
isCancel _     = False

-- Commenting out to avoid compile warnings until fn is needed
-- isect :: Eq a => [a] -> [a] -> Bool
-- [] `isect` _ = False
-- (e:ee) `isect` l = e `elem` l || ee `isect` l

-- Commenting out to avoid compile warnings until fn is needed
-- escape2rx :: String -> String
-- escape2rx []       = []
-- escape2rx ('^':cs) = '\\':'^':escape2rx cs
-- escape2rx (c:cs)   = '[':c:']':escape2rx cs


-- ---------------------------------------------------------------------

-- Query support


simpleq :: String -> String -> (String -> Action) -> JoeMode
simpleq prompt initialValue act = do
  s <- echoMode prompt initialValue
  maybe (return ()) (write . act) s

-- | A simple line editor. 
-- @echoMode prompt exitProcess@ runs the line editor; @prompt@ will
-- be displayed as prompt, @exitProcess@ is a process that will be
-- used to exit the line-editor sub-process if it succeeds on input
-- typed during edition.

echoMode :: String -> String -> JoeProc (Maybe String)
echoMode prompt initialValue = do 
  write (logPutStrLn "echoMode")
  result <- lineEdit initialValue
  return result
    where lineEdit s =
              do write $ msgE (prompt ++ s)
                 choice [satisfy isEnter >> return (Just s),
                         satisfy isCancel >> return Nothing,
                         satisfy isDel >> lineEdit (take (length s - 1) s),
                         do c <- satisfy validChar; lineEdit (s++[c])]

-- Commenting out to avoid compile warnings until fn is needed
-- query :: String -> [(String, JoeMode)] -> JoeMode
-- query prompt ks = write (msgE prompt) >> loop
--     where loop = choice $ (satisfy (isEnter ||| isCancel) >> return ()) :
--                           [oneOf cs >> a | (cs,a) <- ks]
--                           ++ [(anyEvent >> loop)]
--           (|||) = liftM2 (||)

-- Commenting out to avoid compile warnings until fn is needed
-- queryKeys :: String -> [(String, Action)] -> JoeMode
-- queryKeys prompt ks = query prompt [(cs,write a) | (cs,a) <- ks]


-- ---------------------------------------------------------------------

-- Some queries

queryNewE, querySaveE, queryGotoLineE, queryInsertFileE,
    queryBufW :: JoeMode

-- querySearchRepE, nextSearchRepE :: JoeMode


queryNewE = simpleq "File name: " [] fnewE
queryGotoLineE = simpleq "Line number: " [] (gotoLnE . read)
queryInsertFileE = simpleq "File name: " [] insertFileE
queryBufW = simpleq "Buffer: " [] unimplementedQ


-- TODO: this could either use the method in the Nano keymap or the Emacs keymap. 
-- (metaM used change the current keymap)
querySaveE = return ()
--querySaveE = write $
--    getFileE >>= \f -> metaM $ runProc $ (simpleq "File name: " f fwriteToE >> klist)



-- ---------------------------------------------------------------------

-- Search queries
-- TODO: search is currently broken in Core anyway; to re-implement when it gets fixed.
{-

queryReplace :: SearchMatch
             -> String
             -> IO (Maybe SearchMatch)
             -> JoeMode
queryReplace m s sfn =
    queryKeys "Replace? (Y)es (N)o (R)est? " [("yY", repl m False), ("rR", repl m True), ("nN", skip m)]
    where
	skip (_, j) st _ = return $ do
	    res <- do_next j
            case res of
                Nothing -> metaM keymap
                Just p -> metaM $ runProc $ queryReplace p s sfn
	
	repl mm_ rest_ st_ cs_ = return $ repl_ mm_ rest_ st_ cs_
	   where
	       repl_ mm@(i, _) rest st cs = do
	           do_replace mm s
                   res <- do_next (i+length s)
                   case (res, rest) of
                       (Nothing, _)    -> metaM keymap
                       (Just p, True)  -> repl_ p rest st cs
                       (Just p, False) -> metaM $ runProc $ queryReplace p s sfn

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
	
doSearch :: SearchExp -> Action
doSearch srchexp = do
    res <- sfn
    case res of
        Nothing -> errorE "Not found." >> metaM keymap
	Just p -> case js_search_replace st of
            Just rep -> metaM $ runProc $ queryReplace p rep (sfn)
            Nothing -> metaM $ keymap
    where
        sfn = do
            op <- getPointE
            res <- searchDoE srchexp (js_search_dir st)
	    case res of
                Just (Left _) -> gotoPointE op >> return Nothing
                Just (Right p) -> return (Just p)
		Nothing -> return Nothing


mksearch :: String -> String -> Maybe String -> JoeMode
mksearch s flags repl st _ = return $ do
    srchexp <- searchInitE searchrx (js_search_flags newst)
    doSearch srchexp newst
    where
        ignore   = if isect "iI" flags then [IgnoreCase] else []
        dir      = if isect "bB" flags then GoLeft else GoRight
        searchrx = if isect "xX" flags then s else escape2rx s
        newst = st{
            js_search_dir = dir,
            js_search_flags = ignore,
            js_search_replace = repl
            }

querySearchRepE =
    query "Search term: " [] qflags
    where
        flagprompt = "(I)gnore, (R)eplace, (B)ackward Reg.E(x)p? "
        qflags s = query flagprompt [] (qreplace s)
	qreplace s flags | isect "rR" flags =
	    query "Replace with: " [] (mksearch s flags . Just)
	qreplace s flags =
	    mksearch s flags Nothing

nextSearchRepE =
    getRegexE >>= \e -> case e of
        Nothing -> metaM $ runProc $ querySearchRepE
        Just se -> doSearch se
-}

unimplementedQ :: String -> Action
unimplementedQ _ = nopE
