{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2005, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007 Jean-Philippe Bernardy

-- | Search/Replace functions

module Yi.Search (
        setRegexE,      -- :: SearchExp -> EditorM ()
        getRegexE,      -- :: EditorM (Maybe SearchExp)
        SearchMatch,
        SearchF(..),
        searchAndRepLocal,  -- :: String -> String -> IO Bool
        doSearch,            -- :: (Maybe String) -> [SearchF]
                            -- -> Direction -> YiM ()
        searchInit,        -- :: String
                            -- -> [SearchF]
                            -- -> IO SearchExp
        continueSearch,          -- :: SearchExp
                            -- -> IO SearchResult

        -- * Incremental Search

        isearchInitE,
        isearchIsEmpty,
        isearchAddE,
        isearchPrevE,
        isearchNextE,
        isearchWordE,
        isearchHistory,
        isearchDelE,
        isearchCancelE,
        isearchFinishE,

        -- * Replace
        qrNext,
        qrReplaceOne,
        qrFinish,
                 ) where

import Prelude ()
import Yi.Prelude
import Yi.Buffer
import Yi.Buffer.Normal
import Yi.Buffer.HighLevel
import Yi.Regex
import Yi.Buffer.Region
import Yi.Editor
import qualified Yi.Editor as Editor

import Data.Char
import Data.Maybe
import Data.Either
import Data.List (span, takeWhile)
import Data.Typeable

import Control.Monad.State

import Yi.Core
import Yi.History

-- ---------------------------------------------------------------------
-- Searching and substitutions with regular expressions
--
-- The most recent regex is held by the editor. You can get at it with
-- getRegeE. This is useful to determine if there was a previous
-- pattern.
--

-- | Put regex into regex 'register'
setRegexE :: SearchExp -> EditorM ()
setRegexE re = setA regexA (Just re)

-- | Return contents of regex register
getRegexE :: EditorM (Maybe SearchExp)
getRegexE = getA regexA


-- ---------------------------------------------------------------------
--
-- | Global searching. Search for regex and move point to that position.
-- @Nothing@ means reuse the last regular expression. @Just s@ means use
-- @s@ as the new regular expression. Direction of search can be
-- specified as either @Backward@ or @Forward@ (forwards in the buffer).
-- Arguments to modify the compiled regular expression can be supplied
-- as well.
--

type SearchMatch = Region
type SearchResult = Maybe (Either SearchMatch SearchMatch)

doSearch :: (Maybe String)       -- ^ @Nothing@ means used previous
                                -- pattern, if any. Complain otherwise.
                                -- Use getRegexE to check for previous patterns
        -> [SearchF]            -- ^ Flags to modify the compiled regex
        -> Direction            -- ^ @Backward@ or @Forward@
        -> EditorM ()

doSearch s fs d =
     case s of
        Just re -> searchInit re d fs >>= withBuffer0 . continueSearch >>= f
        Nothing -> do
            mre <- getRegexE
            case mre of
                Nothing -> fail "No previous search pattern" -- NB
                Just r -> withBuffer0 (continueSearch (r,d)) >>= f
    where
        f mp = case mp of
            Just (Right _) -> return ()
            Just (Left  _) -> printMsg "Search wrapped"
            Nothing        -> fail "Pattern not found"


-- | Set up a search.
searchInit :: String -> Direction -> [SearchF] -> EditorM (SearchExp, Direction)
searchInit re d fs = do
    let Just c_re = makeSearchOptsM fs re
    setRegexE c_re
    setA searchDirectionA d
    return (c_re,d)

-- | Do a forward search, placing cursor at first char of pattern, if found.
-- Keymaps may implement their own regex language. How do we provide for this?
-- Also, what's happening with ^ not matching sol?
continueSearch :: (SearchExp, Direction) -> BufferM SearchResult
continueSearch (c_re, dir) = do
  mp <- savingPointB $ do
    moveB Character dir  -- start immed. after cursor
    rs <- regexB dir c_re
    moveB Document (reverseDir dir) -- wrap around 
    ls <- regexB dir c_re
    return $ listToMaybe $ fmap Right rs ++ fmap Left ls
  maybe (return ()) (moveTo . regionStart . either id id) mp
  return mp

------------------------------------------------------------------------
-- Global search and replace
--


------------------------------------------------------------------------
-- | Search and replace /on current line/. Returns Bool indicating
-- success or failure
-- TODO: simplify
searchAndRepLocal :: String -> String -> EditorM Bool
searchAndRepLocal [] _ = return False   -- hmm...
searchAndRepLocal re str = do
    let Just c_re = makeSearchOptsM [] re
    setRegexE c_re     -- store away for later use
    setA searchDirectionA Forward
    mp <- withBuffer0 $ regexB Forward c_re   -- find the regex
    case mp of
        (r:_) -> withBuffer0 $ savingPointB $ do
                moveToEol
                ep <- pointB      -- eol point of current line
                moveTo $ regionStart r
                moveToEol
                eq <- pointB      -- eol of matched line
                if (ep /= eq)       -- then match isn't on current line
                    then return False
                    else do         -- do the replacement
                replaceRegionB r str
                return True -- signal success
        [] -> return False


--------------------------
-- Incremental search


newtype Isearch = Isearch [(String, Region, Direction)] deriving Typeable
-- This contains: (string currently searched, position where we
-- searched it, direction, overlay for highlighting searched text)

-- Maybe this should not be saved in a Dynamic component!
-- it could also be embedded in the Keymap state.

instance Initializable Isearch where
    initial = (Isearch [])

isearchInitE :: Direction -> EditorM ()
isearchInitE dir = do
  historyStartGen iSearch
  p <- withBuffer0 pointB
  setRegexE emptySearch
  setDynamic (Isearch [("",mkRegion p p,dir)])
  printMsg "I-search: "

isearchIsEmpty :: EditorM Bool
isearchIsEmpty = do
  Isearch s <- getDynamic
  return $ not $ null $ fst3 $ head $ s

isearchAddE :: String -> EditorM ()
isearchAddE increment = isearchFunE (++ increment)

makeISearch :: String -> (String, Regex)
makeISearch s = case makeSearchOptsM opts s of
                  Nothing -> (s, emptyRegex)
                  Just search -> search
   where opts = if any isUpper s then [] else [IgnoreCase]

isearchFunE :: (String -> String) -> EditorM ()
isearchFunE fun = do
  Isearch s <- getDynamic
  let (previous,p0,direction) = head s
  let current = fun previous
  printMsg $ "I-search: " ++ current
  prevPoint <- withBuffer0 pointB
  withBuffer0 $ do
    moveTo $ regionStart p0
  let srch = makeISearch current
  setRegexE srch
  mp <- withBuffer0 $ regexB direction srch
  case mp of
    [] -> do withBuffer0 $ moveTo prevPoint -- go back to where we were
             setDynamic $ Isearch ((current,p0,direction):s)
             printMsg $ "Failing I-search: " ++ current
    (p:_) -> do
                  withBuffer0 $ do
                    moveTo (regionEnd p)
                  setDynamic $ Isearch ((current,p,direction):s)
                 
isearchDelE :: EditorM ()
isearchDelE = do
  Isearch s <- getDynamic
  case s of
    (_:(text,p,dir):rest) -> do
      withBuffer0 $ do
        moveTo $ regionEnd p
      setDynamic $ Isearch ((text,p,dir):rest)
      setRegexE $ makeISearch $ text
      printMsg $ "I-search: " ++ text
    _ -> return () -- if the searched string is empty, don't try to remove chars from it.

isearchHistory :: Int -> EditorM ()
isearchHistory delta = do
  Isearch ((current,_p0,_dir):_) <- getDynamic
  h <- historyMoveGen iSearch delta (return current)
  isearchFunE (const h)

isearchPrevE :: EditorM ()
isearchPrevE = isearchNext0 Backward

isearchNextE :: EditorM ()
isearchNextE = isearchNext0 Forward

isearchNext0 :: Direction -> EditorM ()
isearchNext0 newDir = do
  Isearch ((current,_p0,_dir):_rest) <- getDynamic
  if null current
    then isearchHistory 1
    else isearchNext newDir
     

isearchNext :: Direction -> EditorM ()
isearchNext direction = do
  Isearch ((current,p0,_dir):rest) <- getDynamic
  withBuffer0 $ moveTo (regionStart p0 + startOfs)
  mp <- withBuffer0 $ do
    regexB direction (makeISearch current)
  case mp of
    [] -> do 
                  endPoint <- withBuffer0 $ do 
                          moveTo (regionEnd p0) -- revert to offset we were before.
                          sizeB   
                  printMsg $ "isearch: end of document reached"
                  let wrappedOfs = case direction of
                                     Forward -> mkRegion 0 0
                                     Backward -> mkRegion endPoint endPoint
                  setDynamic $ Isearch ((current,wrappedOfs,direction):rest) -- prepare to wrap around.
    (p:_) -> do   
                  withBuffer0 $ do
                    moveTo (regionEnd p)
                  setDynamic $ Isearch ((current,p,direction):rest)
 where startOfs = case direction of
                      Forward  ->  1
                      Backward -> -1

isearchWordE :: EditorM ()
isearchWordE = do
  text <- withBuffer0 (pointB >>= nelemsB 32) -- add maximum 32 chars at a time.
  let (prefix, rest) = span (not . isAlpha) text
      word = takeWhile isAlpha rest
  isearchAddE (prefix ++ word)

isearchFinishE :: EditorM ()
isearchFinishE = isearchEnd True

isearchCancelE :: EditorM ()
isearchCancelE = isearchEnd False

iSearch :: String
iSearch = "isearch"

isearchEnd :: Bool -> EditorM ()
isearchEnd accept = do
  Isearch s <- getDynamic
  let (lastSearched,_,_) = head s
  let (_,p0,_) = last s
  historyFinishGen iSearch (return lastSearched)
  setRegexE emptySearch
  if accept 
     then do withBuffer0 $ setSelectionMarkPointB $ regionStart p0 
             printMsg "Quit"
     else withBuffer0 $ moveTo $ regionStart p0
  
  

-----------------
-- Query-Replace

qrNext :: BufferRef -> SearchExp -> EditorM ()
qrNext b what = do
  mp <- withGivenBuffer0 b $ regexB Forward what
  case mp of
    [] -> do
      printMsg "String to search not found"
      qrFinish
    (r:_) -> withGivenBuffer0 b $ setSelectRegionB r

qrFinish :: EditorM ()
qrFinish = do
  setA regexA Nothing
  closeBufferAndWindowE  -- the minibuffer.
  

qrReplaceOne :: BufferRef -> SearchExp -> String -> EditorM ()
qrReplaceOne b reg replacement = do
  withGivenBuffer0 b $ do
    r <- getRawSelectRegionB
    replaceRegionB r replacement
  qrNext b reg

