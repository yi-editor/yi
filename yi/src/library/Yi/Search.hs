{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2005, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007 Jean-Philippe Bernardy

-- | Search/Replace functions

module Yi.Search (
        setRegexE,      -- :: SearchExp -> EditorM ()
        resetRegexE,    -- :: EditorM ()
        getRegexE,      -- :: EditorM (Maybe SearchExp)
        SearchMatch,
        SearchResult(..),
        SearchOption(..),
        doSearch,            -- :: (Maybe String) -> [SearchOption]
                            -- -> Direction -> YiM ()
        searchInit,        -- :: String
                            -- -> [SearchOption]
                            -- -> IO SearchExp
        continueSearch,          -- :: SearchExp
                            -- -> IO SearchResult

        -- * Batch search-replace
        searchReplaceRegionB,
        searchReplaceSelectionB,
        replaceString,
        searchAndRepRegion,
        searchAndRepUnit, -- :: String -> String -> Bool -> TextUnit -> EditorM Bool
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
import Data.List (span, takeWhile, take, length)
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
setRegexE re = putA currentRegexA (Just re)

-- | Clear the regex 'register'
resetRegexE :: EditorM ()
resetRegexE = putA currentRegexA Nothing

-- | Return contents of regex register
getRegexE :: EditorM (Maybe SearchExp)
getRegexE = getA currentRegexA


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
  deriving Eq

doSearch :: Maybe String        -- ^ @Nothing@ means used previous
                                -- pattern, if any. Complain otherwise.
                                -- Use getRegexE to check for previous patterns
        -> [SearchOption]            -- ^ Flags to modify the compiled regex
        -> Direction            -- ^ @Backward@ or @Forward@
        -> EditorM SearchResult

doSearch (Just re) fs d = searchInit re d fs >>= withBuffer0 . continueSearch
doSearch Nothing   _  d = do
  mre <- getRegexE
  case mre of
    Nothing -> fail "No previous search pattern" -- NB
    Just r  -> withBuffer0 (continueSearch (r,d))

-- | Set up a search.
searchInit :: String -> Direction -> [SearchOption] -> EditorM (SearchExp, Direction)
searchInit re d fs = do
    let Right c_re = makeSearchOptsM fs re
    setRegexE c_re
    putA searchDirectionA d
    return (c_re,d)

-- | Do a search, placing cursor at first char of pattern, if found.
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
-- Batch search and replace
--

-- | Search and Replace all within the current region.
-- Note the region is the final argument since we might perform
-- the same search and replace over multiple regions however we are
-- unlikely to perform several search and replaces over the same region
-- since the first such may change the bounds of the region.
searchReplaceRegionB ::
                       String -- ^ The String to search for
                    -> String -- ^ The String to replace it with
                    -> Region -- ^ The region to perform this over
                    -> BufferM Int
searchReplaceRegionB from to = searchAndRepRegion0 (makeSimpleSearch from) to True


-- | Peform a search and replace on the selection
searchReplaceSelectionB ::
                           String -- ^ The String to search for
                        -> String  -- ^ The String to replace it with
                        -> BufferM Int
searchReplaceSelectionB from to = searchReplaceRegionB from to =<< getSelectRegionB

-- | Replace a string by another everywhere in the document
replaceString :: String -> String -> BufferM Int
replaceString a b = searchReplaceRegionB a b =<< regionOfB Document

------------------------------------------------------------------------
-- | Search and replace in the given region.
-- If the input boolean is True, then the replace is done globally, otherwise only the first match is replaced.
-- Returns the number of replacements done.
searchAndRepRegion0 :: SearchExp -> String -> Bool -> Region -> BufferM Int
searchAndRepRegion0 c_re str globally region = do
    mp <- (if globally then id else take 1) <$> regexRegionB c_re region  -- find the regex
    -- mp' is a maybe not reversed version of mp, the goal
    -- is to avoid replaceRegionB to mess up the next regions.
    -- So we start from the end.
    let mp' = mayReverse (reverseDir $ regionDirection region) mp
    mapM_ (`replaceRegionB` str) mp'
    return (length mp)

searchAndRepRegion :: String -> String -> Bool -> Region -> EditorM Bool
searchAndRepRegion [] _ _ _ = return False   -- hmm...
searchAndRepRegion s str globally region = do
    let c_re = makeSimpleSearch s
    setRegexE c_re     -- store away for later use
    putA searchDirectionA Forward
    withBuffer0 $ (/= 0) <$> searchAndRepRegion0 c_re str globally region

------------------------------------------------------------------------
-- | Search and replace in the region defined by the given unit.
-- The rest is as in 'searchAndRepRegion'.
searchAndRepUnit :: String -> String -> Bool -> TextUnit -> EditorM Bool
searchAndRepUnit re str g unit = searchAndRepRegion re str g =<< (withBuffer0 $ regionOfB unit)

--------------------------
-- Incremental search


newtype Isearch = Isearch [(String, Region, Direction)] 
  deriving (Typeable, Binary)
-- This contains: (string currently searched, position where we
-- searched it, direction, overlay for highlighting searched text)

-- Note that this info cannot be embedded in the Keymap state: the state
-- modification can depend on the state of the editor.

instance Initializable Isearch where
    initial = (Isearch [])

instance YiVariable Isearch

isearchInitE :: Direction -> EditorM ()
isearchInitE dir = do
  historyStartGen iSearch
  p <- withBuffer0 pointB
  resetRegexE 
  setDynamic (Isearch [("",mkRegion p p,dir)])
  printMsg "I-search: "

isearchIsEmpty :: EditorM Bool
isearchIsEmpty = do
  Isearch s <- getDynamic
  return $ not $ null $ fst3 $ head $ s

isearchAddE :: String -> EditorM ()
isearchAddE increment = isearchFunE (++ increment)

-- | Create a SearchExp that matches exactly its argument
makeSimpleSearch :: String -> SearchExp
makeSimpleSearch s = se
    where Right se = makeSearchOptsM [QuoteRegex] s

makeISearch :: String -> SearchExp
makeISearch s = case makeSearchOptsM opts s of
                  Left _ -> SearchExp s emptyRegex emptyRegex
                  Right search -> search
   where opts = QuoteRegex : if any isUpper s then [] else [IgnoreCase]

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
  if accept 
     then do withBuffer0 $ setSelectionMarkPointB $ regionStart p0 
             printMsg "Quit"
     else do resetRegexE
             withBuffer0 $ moveTo $ regionStart p0


-----------------
-- Query-Replace

-- | Find the next match and select it.
-- Point is end, mark is beginning.
qrNext :: Window -> BufferRef -> SearchExp -> EditorM ()
qrNext win b what = do
  mp <- withGivenBufferAndWindow0 win b $ regexB Forward what
  case mp of
    [] -> do
      printMsg "String to search not found"
      qrFinish
    (r:_) -> withGivenBufferAndWindow0 win b $ setSelectRegionB r

-- | Replace all the remaining occurrences.
qrReplaceAll :: Window -> BufferRef -> SearchExp -> String -> EditorM ()
qrReplaceAll win b what replacement = do
     n <- withGivenBufferAndWindow0 win b $ do 
         exchangePointAndMarkB -- so we replace the current occurence too
         searchAndRepRegion0 what replacement True =<< regionOfPartB Document Forward
     printMsg $ "Replaced " ++ show n  ++ " occurrences"
     qrFinish

-- |  Exit from query/replace.
qrFinish :: EditorM ()
qrFinish = do
  putA currentRegexA Nothing
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
  withGivenBufferAndWindow0 win b $ do
   flip replaceRegionB replacement =<< getRawestSelectRegionB

