{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Search
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Search/Replace functions

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
        makeSimpleSearch,

        -- * Batch search-replace
        searchReplaceRegionB,
        searchReplaceSelectionB,
        replaceString,
        searchAndRepRegion,
        searchAndRepRegion0,
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
        isearchCancelWithE,
        isearchFinishWithE,

        -- * Replace
        qrNext,
        qrReplaceAll,
        qrReplaceOne,
        qrFinish
                 ) where

import           Control.Applicative
import           Control.Lens hiding (re, from, to, act)
import           Control.Monad
import           Data.Binary
import           Data.Char
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Typeable
import           Yi.Buffer
import           Yi.Types (YiVariable)
import           Yi.Editor
import           Yi.History
import           Yi.Regex
import qualified Yi.Rope as R
import           Yi.Search.Internal
import           Yi.String (showT)
import           Yi.Utils
import           Yi.Window

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
    assign searchDirectionA d
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
searchReplaceRegionB :: R.YiString -- ^ The string to search for
                     -> R.YiString -- ^ The string to replace it with
                     -> Region -- ^ The region to perform this over
                     -> BufferM Int
searchReplaceRegionB from to =
  searchAndRepRegion0 (makeSimpleSearch from) to True


-- | Peform a search and replace on the selection
searchReplaceSelectionB :: R.YiString  -- ^ text to search for
                        -> R.YiString  -- ^ text to replace it with
                        -> BufferM Int
searchReplaceSelectionB from to =
  getSelectRegionB >>= searchReplaceRegionB from to

-- | Replace a string by another everywhere in the document
replaceString :: R.YiString -> R.YiString -> BufferM Int
replaceString a b = regionOfB Document >>= searchReplaceRegionB a b

------------------------------------------------------------------------
-- | Search and replace in the given region.
--
-- If the input boolean is True, then the replace is done globally,
-- otherwise only the first match is replaced. Returns the number of
-- replacements done.
searchAndRepRegion0 :: SearchExp -> R.YiString -> Bool -> Region -> BufferM Int
searchAndRepRegion0 c_re str globally region = do
    mp <- (if globally then id else take 1) <$> regexRegionB c_re region  -- find the regex
    -- mp' is a maybe not reversed version of mp, the goal
    -- is to avoid replaceRegionB to mess up the next regions.
    -- So we start from the end.
    let mp' = mayReverse (reverseDir $ regionDirection region) mp
    mapM_ (`replaceRegionB` str) mp'
    return (length mp)

searchAndRepRegion :: R.YiString -> R.YiString -> Bool -> Region -> EditorM Bool
searchAndRepRegion s str globally region = case R.null s of
  False -> return False
  True -> do
    let c_re = makeSimpleSearch s
    setRegexE c_re     -- store away for later use
    assign searchDirectionA Forward
    withBuffer0 $ (/= 0) <$> searchAndRepRegion0 c_re str globally region

------------------------------------------------------------------------
-- | Search and replace in the region defined by the given unit.
-- The rest is as in 'searchAndRepRegion'.
searchAndRepUnit :: R.YiString -> R.YiString -> Bool -> TextUnit -> EditorM Bool
searchAndRepUnit re str g unit =
  withBuffer0 (regionOfB unit) >>= searchAndRepRegion re str g

--------------------------
-- Incremental search


newtype Isearch = Isearch [(T.Text, Region, Direction)]
  deriving (Typeable, Show)

instance Binary Isearch where
  put (Isearch ts) = put (map3 E.encodeUtf8 ts)
  get = Isearch . map3 E.decodeUtf8 <$> get

map3 :: (a -> d) -> [(a, b, c)] -> [(d, b, c)]
map3 _ [] = []
map3 f ((a, b, c):xs) = (f a, b, c) : map3 f xs


-- This contains: (string currently searched, position where we
-- searched it, direction, overlay for highlighting searched text)

-- Note that this info cannot be embedded in the Keymap state: the state
-- modification can depend on the state of the editor.

instance Default Isearch where
    def = Isearch []

instance YiVariable Isearch

isearchInitE :: Direction -> EditorM ()
isearchInitE dir = do
  historyStartGen iSearch
  p <- withBuffer0 pointB
  resetRegexE
  putEditorDyn (Isearch [(T.empty ,mkRegion p p, dir)])
  printMsg "I-search: "

isearchIsEmpty :: EditorM Bool
isearchIsEmpty = do
  Isearch s <- getEditorDyn
  return . not . T.null . fst3 $ head s

isearchAddE :: T.Text -> EditorM ()
isearchAddE inc = isearchFunE (<> inc)

-- | Create a SearchExp that matches exactly its argument
makeSimpleSearch :: R.YiString -> SearchExp
makeSimpleSearch s = se
    where Right se = makeSearchOptsM [QuoteRegex] (R.toString s)

makeISearch :: T.Text -> SearchExp
makeISearch s = case makeSearchOptsM opts (T.unpack s) of
                  Left _ -> SearchExp (T.unpack s) emptyRegex emptyRegex []
                  Right search -> search
   where opts = QuoteRegex : if T.any isUpper s then [] else [IgnoreCase]

isearchFunE :: (T.Text -> T.Text) -> EditorM ()
isearchFunE fun = do
  Isearch s <- getEditorDyn
  let (previous,p0,direction) = head s
      current = fun previous
      srch = makeISearch current
  printMsg $ "I-search: " <> current
  setRegexE srch
  prevPoint <- withBuffer0 pointB
  matches <- withBuffer0 $ do
      moveTo $ regionStart p0
      when (direction == Backward) $
         moveN $ T.length current
      regexB direction srch

  let onSuccess p = do withBuffer0 $ moveTo (regionEnd p)
                       putEditorDyn $ Isearch ((current,p,direction):s)

  case matches of
    (p:_) -> onSuccess p
    [] -> do matchesAfterWrap <- withBuffer0 $ do
               case direction of
                 Forward -> moveTo 0
                 Backward -> do
                   bufferLength <- sizeB
                   moveTo bufferLength
               regexB direction srch

             case matchesAfterWrap of
               (p:_) -> onSuccess p
               [] -> do withBuffer0 $ moveTo prevPoint -- go back to where we were
                        putEditorDyn $ Isearch ((current,p0,direction):s)
                        printMsg $ "Failing I-search: " <> current

isearchDelE :: EditorM ()
isearchDelE = do
  Isearch s <- getEditorDyn
  case s of
    (_:(text,p,dir):rest) -> do
      withBuffer0 $
        moveTo $ regionEnd p
      putEditorDyn $ Isearch ((text,p,dir):rest)
      setRegexE $ makeISearch text
      printMsg $ "I-search: " <> text
    _ -> return () -- if the searched string is empty, don't try to remove chars from it.

isearchHistory :: Int -> EditorM ()
isearchHistory delta = do
  Isearch ((current,_p0,_dir):_) <- getEditorDyn
  h <- historyMoveGen iSearch delta (return current)
  isearchFunE (const h)

isearchPrevE :: EditorM ()
isearchPrevE = isearchNext0 Backward

isearchNextE :: EditorM ()
isearchNextE = isearchNext0 Forward

isearchNext0 :: Direction -> EditorM ()
isearchNext0 newDir = do
  Isearch ((current,_p0,_dir):_rest) <- getEditorDyn
  if T.null current
    then isearchHistory 1
    else isearchNext newDir


isearchNext :: Direction -> EditorM ()
isearchNext direction = do
  Isearch ((current,p0,_dir):rest) <- getEditorDyn
  withBuffer0 $ moveTo (regionStart p0 + startOfs)
  mp <- withBuffer0 $
    regexB direction (makeISearch current)
  case mp of
    [] -> do
      endPoint <- withBuffer0 $ do
              moveTo (regionEnd p0) -- revert to offset we were before.
              sizeB
      printMsg "isearch: end of document reached"
      let wrappedOfs = case direction of
                         Forward -> mkRegion 0 0
                         Backward -> mkRegion endPoint endPoint
      putEditorDyn $ Isearch ((current,wrappedOfs,direction):rest) -- prepare to wrap around.
    (p:_) -> do
      withBuffer0 $
        moveTo (regionEnd p)
      printMsg $ "I-search: " <> current
      putEditorDyn $ Isearch ((current,p,direction):rest)
 where startOfs = case direction of
                      Forward  ->  1
                      Backward -> -1

isearchWordE :: EditorM ()
isearchWordE = do
   -- add maximum 32 chars at a time.
  text <- R.toText <$> withBuffer0 (pointB >>= nelemsB 32)

  let (prefix, rest) = T.break isAlpha text
      word = T.takeWhile isAlpha rest
  isearchAddE $ prefix <> word

-- | Succesfully finish a search. Also see 'isearchFinishWithE'.
isearchFinishE :: EditorM ()
isearchFinishE = isearchEnd True

-- | Cancel a search. Also see 'isearchCancelWithE'.
isearchCancelE :: EditorM ()
isearchCancelE = isearchEnd False

-- | Wrapper over 'isearchEndWith' that passes through the action and
-- accepts the search as successful (i.e. when the user wants to stay
-- at the result).
isearchFinishWithE :: EditorM a -> EditorM ()
isearchFinishWithE act = isearchEndWith act True

-- | Wrapper over 'isearchEndWith' that passes through the action and
-- marks the search as unsuccessful (i.e. when the user wants to
-- jump back to where the search started).
isearchCancelWithE :: EditorM a -> EditorM ()
isearchCancelWithE act = isearchEndWith act False

iSearch :: T.Text
iSearch = "isearch"

-- | Editor action describing how to end finish incremental search.
-- The @act@ parameter allows us to specify an extra action to run
-- before finishing up the search. For Vim, we don't want to do
-- anything so we use 'isearchEnd' which just does nothing. For emacs,
-- we want to cancel highlighting and stay where we are.
isearchEndWith :: EditorM a -> Bool -> EditorM ()
isearchEndWith act accept = getEditorDyn >>= \case
  Isearch [] -> return ()
  Isearch s@((lastSearched, _, dir):_) -> do
    let (_,p0,_) = last s
    historyFinishGen iSearch (return lastSearched)
    assign searchDirectionA dir
    if accept
       then do void act
               withBuffer0 $ setSelectionMarkPointB $ regionStart p0
               printMsg "Quit"
       else do resetRegexE
               withBuffer0 $ moveTo $ regionStart p0

-- | Specialised 'isearchEndWith' to do nothing as the action.
isearchEnd :: Bool -> EditorM ()
isearchEnd = isearchEndWith (return ())

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
qrReplaceAll :: Window -> BufferRef -> SearchExp -> R.YiString -> EditorM ()
qrReplaceAll win b what replacement = do
  n <- withGivenBufferAndWindow0 win b $ do
    exchangePointAndMarkB -- so we replace the current occurence too
    searchAndRepRegion0 what replacement True =<< regionOfPartB Document Forward
  printMsg $ "Replaced " <> showT n <> " occurrences"
  qrFinish

-- | Exit from query/replace.
qrFinish :: EditorM ()
qrFinish = do
  assign currentRegexA Nothing
  closeBufferAndWindowE  -- the minibuffer.

-- | We replace the currently selected match and then move to the next
-- match.
qrReplaceOne :: Window -> BufferRef -> SearchExp -> R.YiString -> EditorM ()
qrReplaceOne win b reg replacement = do
  qrReplaceCurrent win b replacement
  qrNext win b reg

-- | This may actually be a bit more general it replaces the current
-- selection with the given replacement string in the given window and
-- buffer.
qrReplaceCurrent :: Window -> BufferRef -> R.YiString -> EditorM ()
qrReplaceCurrent win b replacement =
  withGivenBufferAndWindow0 win b $
   flip replaceRegionB replacement =<< getRawestSelectRegionB
