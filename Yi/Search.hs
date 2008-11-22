{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2005, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007 Jean-Philippe Bernardy

-- | Search/Replace functions

module Yi.Search (
        setRegexE,      -- :: SearchExp -> EditorM ()
        getRegexE,      -- :: EditorM (Maybe SearchExp)
        SearchMatch,
        SearchResult(..),
        SearchF(..),
        searchAndRepRegion,  -- :: String -> String -> Bool -> Region -> EditorM Bool
        searchAndRepUnit, -- :: String -> String -> Bool -> TextUnit -> EditorM Bool
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
        qrReplaceAll,
        qrReplaceOne,
        qrFinish,
                 ) where

import Prelude ()
import Yi.Regex
import Yi.Window
import Data.Char
import Data.Maybe
import Data.Either
import Data.List (span, takeWhile, take, filter, length)

import Yi.Core
import Yi.Core as Editor
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
data SearchResult = PatternFound
                  | PatternNotFound
                  | SearchWrapped

doSearch :: (Maybe String)       -- ^ @Nothing@ means used previous
                                -- pattern, if any. Complain otherwise.
                                -- Use getRegexE to check for previous patterns
        -> [SearchF]            -- ^ Flags to modify the compiled regex
        -> Direction            -- ^ @Backward@ or @Forward@
        -> EditorM SearchResult

doSearch (Just re) fs d = searchInit re d fs >>= withBuffer0 . continueSearch
doSearch Nothing   _  d = do
  mre <- getRegexE
  case mre of
    Nothing -> fail "No previous search pattern" -- NB
    Just r  -> withBuffer0 (continueSearch (r,d))

-- | Set up a search.
searchInit :: String -> Direction -> [SearchF] -> EditorM (SearchExp, Direction)
searchInit re d fs = do
    let Right c_re = makeSearchOptsM fs re
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
  return $ f mp
    where
        f (Just (Right _)) = PatternFound
        f (Just (Left  _)) = SearchWrapped
        f Nothing          = PatternNotFound

------------------------------------------------------------------------
-- Global search and replace
--


------------------------------------------------------------------------
-- | Search and replace in the given region.
-- If the input boolean is True, then the replace is done globally.
-- Returns Bool indicating success or failure.
searchAndRepRegion :: String -> String -> Bool -> Region -> EditorM Bool
searchAndRepRegion [] _ _ _ = return False   -- hmm...
searchAndRepRegion re str globally region = do
    let Right c_re = makeSearchOptsM [] re
    setRegexE c_re     -- store away for later use
    setA searchDirectionA Forward
    mp <- withBuffer0 $
            (if globally then id else take 1) <$>
               filter (`includedRegion` region) <$>
                  regexRegionB c_re region  -- find the regex
    -- mp' is a maybe not reversed version of mp, the goal
    -- is to avoid replaceRegionB to mess up the next regions.
    -- So we start from the end.
    let mp' = mayReverse (reverseDir $ regionDirection region) mp
    withBuffer0 $ mapM_ (`replaceRegionB` str) mp'
    return (not $ null mp)

------------------------------------------------------------------------
-- | Search and replace in the region defined by the given unit.
-- The rest is as in 'searchAndRepRegion'.
searchAndRepUnit :: String -> String -> Bool -> TextUnit -> EditorM Bool
searchAndRepUnit re str g unit = searchAndRepRegion re str g =<< (withBuffer0 $ regionOfB unit)

--------------------------
-- Incremental search


newtype Isearch = Isearch [(String, Region, Direction)] deriving Typeable
-- This contains: (string currently searched, position where we
-- searched it, direction, overlay for highlighting searched text)

-- Note that this info cannot be embedded in the Keymap state: the state
-- modification can depend on the state of the editor.

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
                  Left _ -> (s, emptyRegex)
                  Right search -> search
   where opts = if any isUpper s then [] else [IgnoreCase]

isearchFunE :: (String -> String) -> EditorM ()
isearchFunE fun = do
  Isearch s <- getDynamic
  let (previous,p0,direction) = head s
      current = fun previous
      srch = makeISearch current
  printMsg $ "I-search: " ++ current
  setRegexE srch
  prevPoint <- withBuffer0 pointB
  mp <- withBuffer0 $ do
      moveTo $ regionStart p0
      when (direction == Backward) $
         moveN $ length current
      regexB direction srch
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
                  printMsg $ "I-search: " ++ current
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

{-
  Find the next match and highlight it.
-}
qrNext :: Window -> BufferRef -> SearchExp -> EditorM ()
qrNext win b what = do
  mp <- withGivenBufferAndWindow0 win b $ regexB Forward what
  case mp of
    [] -> do
      printMsg "String to search not found"
      qrFinish
    (r:_) -> withGivenBufferAndWindow0 win b $ setSelectRegionB r

{- Replace all the remaining occurrences.
   We might think that since 'regexB' returns all the remaining
   matches that we could replace them all without re-searching.
   However this would work only if the replacement was the same
   length as the pattern being replaced.
-}
qrReplaceAll :: Window -> BufferRef -> SearchExp -> String -> EditorM ()
qrReplaceAll win b what replacement =
  do -- We must first replace the current match
     qrReplaceCurrent win b replacement
     qrAllRemaining 
  where
  qrAllRemaining =
    do mp <- withGivenBufferAndWindow0 win b $ regexB Forward what
       case mp of
         []    -> do printMsg "Replaced all occurrences"
                     qrFinish
         (r:_) -> do replaceAction r
                     qrAllRemaining
  replaceAction r = withGivenBufferAndWindow0 win b $ 
                    replaceRegionB r replacement

{-
  Exit from query/replace.
-}
qrFinish :: EditorM ()
qrFinish = do
  setA regexA Nothing
  closeBufferAndWindowE  -- the minibuffer.
  
{-
  We replace the currently selected match and then move to the next
  match.
-}
qrReplaceOne :: Window -> BufferRef -> SearchExp -> String -> EditorM ()
qrReplaceOne win b reg replacement =
  do qrReplaceCurrent win b replacement
     qrNext win b reg

{- This may actually be a bit more general it replaces the current
   selection with the given replacement string in the given
   window and buffer. 
-}
qrReplaceCurrent :: Window -> BufferRef -> String -> EditorM ()
qrReplaceCurrent win b replacement = 
  withGivenBufferAndWindow0 win b $ modifySelectionB (const replacement)
