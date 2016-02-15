{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Snippets
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Snippets where

import           GHC.Generics (Generic)

import           Control.Applicative (some)
import           Control.Arrow       (second)
import           Control.Lens        (use, (.=))
import           Control.Monad.RWS   (MonadPlus (mplus), MonadReader (ask),
                                      MonadState, MonadTrans (..),
                                      MonadWriter (tell),
                                      Monoid (mappend, mempty), RWST, evalRWST,
                                      filterM, forM, forM_, liftM2, unless,
                                      when, (<>))
import           Data.Binary         (Binary)
import           Data.Char           (isSpace)
import           Data.Default        (Default, def)
import           Data.Foldable       (find)
import           Data.List           (foldl', groupBy, intersperse, nub, sort)
import           Data.Maybe          (catMaybes)
import qualified Data.Text           as T (Text)
import           Data.Typeable       (Typeable)
import           Yi.Buffer
import           Yi.Editor           (withCurrentBuffer)
import           Yi.Keymap           (Action)
import           Yi.Keymap.Keys
import qualified Yi.Rope             as R
import           Yi.TextCompletion   (resetComplete, wordCompleteString')
import           Yi.Types            (YiVariable)

type SnippetCmd = RWST (Int, Int) [MarkInfo] () BufferM

data SnippetMark = SimpleMark !Int
                 | ValuedMark !Int R.YiString
                 | DependentMark !Int


data MarkInfo = SimpleMarkInfo { userIndex :: !Int
                               , startMark :: !Mark }
              | ValuedMarkInfo { userIndex :: !Int
                               , startMark :: !Mark
                               , endMark :: !Mark }
              | DependentMarkInfo { userIndex :: !Int
                                  , startMark :: !Mark
                                  , endMark :: !Mark }
  deriving (Eq, Show, Generic)

instance Binary MarkInfo

newtype BufferMarks = BufferMarks { bufferMarks :: [MarkInfo] }
  deriving (Eq, Show, Monoid, Typeable, Binary)

newtype DependentMarks = DependentMarks { marks :: [[MarkInfo]] }
  deriving (Eq, Show, Monoid, Typeable, Binary)

instance Default BufferMarks where
  def = BufferMarks []

instance Default DependentMarks where
  def = DependentMarks []

instance YiVariable BufferMarks
instance YiVariable DependentMarks

instance Ord MarkInfo where
  a `compare` b = userIndex a `compare` userIndex b

cursor :: Int -> SnippetMark
cursor = SimpleMark

cursorWith :: Int -> R.YiString -> SnippetMark
cursorWith = ValuedMark

dep :: Int -> SnippetMark
dep = DependentMark

isDependentMark :: MarkInfo -> Bool
isDependentMark (SimpleMarkInfo{})    = False
isDependentMark (ValuedMarkInfo{})    = False
isDependentMark (DependentMarkInfo{}) = True

bufferMarkers :: MarkInfo -> [Mark]
bufferMarkers (SimpleMarkInfo _ s) = [s]
bufferMarkers m = [startMark m, endMark m]

-- used to translate a datatype into a snippet cmd for
-- freely combining data with '&'
class MkSnippetCmd a b | a -> b where
  mkSnippetCmd :: a -> SnippetCmd b

instance MkSnippetCmd String () where
  mkSnippetCmd = text . R.fromString

instance MkSnippetCmd R.YiString () where
  mkSnippetCmd = text

instance MkSnippetCmd T.Text () where
  mkSnippetCmd = text . R.fromText

instance MkSnippetCmd (SnippetCmd a) a where
  mkSnippetCmd = id

-- mkSnippetCmd for 'cursor...'-functions
instance MkSnippetCmd SnippetMark () where
  mkSnippetCmd (SimpleMark i) = do
      mk <- mkMark
      tell [SimpleMarkInfo i mk]

  mkSnippetCmd (ValuedMark i str) = do
      start <- mkMark
      lift $ insertN str
      end <- mkMark
      tell [ValuedMarkInfo i start end]

  mkSnippetCmd (DependentMark i) = do
      start <- mkMark
      end <- mkMark
      tell [DependentMarkInfo i start end]

-- create a mark at current position
mkMark :: MonadTrans t => t BufferM Mark
mkMark = lift $ do p <- pointB
                   newMarkB $ MarkValue p Backward

-- Indentation support has been temporarily removed
text :: R.YiString -> SnippetCmd ()
text txt = do
  (_, indent) <- ask
  indentSettings <- lift indentSettingsB
  lift . foldl' (>>) (return ()) .
      intersperse (newlineB >> indentToB indent) .
      map (if expandTabs indentSettings
           then insertN . expand indentSettings ""
           else insertN) $ lines' txt
  where
    lines' txt' = case R.last txt' of
      Just '\n' -> R.lines txt' <> [mempty]
      _         -> R.lines txt

    expand :: IndentSettings -> R.YiString -> R.YiString -> R.YiString
    expand is str rst = case R.head rst of
      Nothing -> R.reverse str
      Just '\t' -> let t = R.replicateChar (tabSize is) ' ' <> str
                   in expand is t (R.drop 1 rst)
      Just s -> expand is (s `R.cons` str) rst

-- unfortunatelly data converted to snippets are no monads, but '&' is
-- very similar to '>>' and '&>' is similar to '>>=', since
-- SnippetCmd's can be used monadically
infixr 5 &
(&) :: (MkSnippetCmd a any , MkSnippetCmd b c) => a -> b -> SnippetCmd c
str & rst = mkSnippetCmd str >> mkSnippetCmd rst

(&>) :: (MkSnippetCmd a b, MkSnippetCmd c d) => a -> (b -> c) -> SnippetCmd d
str &> rst = mkSnippetCmd str >>= mkSnippetCmd . rst

runSnippet :: Bool -> SnippetCmd a -> BufferM a
runSnippet deleteLast s = do
    line <- lineOf =<< pointB
    indent <- indentOfCurrentPosB
    (a, markInfo) <- evalRWST s (line, indent) ()
    unless (null markInfo) $ do
        let newMarks = sort $ filter (not . isDependentMark) markInfo
        let newDepMarks = filter (not . len1) $
                            groupBy belongTogether $
                              sort markInfo
        getBufferDyn >>= putBufferDyn.(BufferMarks newMarks `mappend`)
        unless (null newDepMarks) $
            getBufferDyn >>= putBufferDyn.(DependentMarks newDepMarks `mappend`)
        moveToNextBufferMark deleteLast
    return a
  where
    len1 (_:[]) = True
    len1 _      = False

    belongTogether a b = userIndex a == userIndex b

updateUpdatedMarks :: [Update] -> BufferM ()
updateUpdatedMarks upds = findEditedMarks upds >>=
                          mapM_ updateDependents

findEditedMarks :: [Update] -> BufferM [MarkInfo]
findEditedMarks upds = fmap (nub . concat) (mapM findEditedMarks' upds)
  where
    findEditedMarks' :: Update -> BufferM [MarkInfo]
    findEditedMarks' upd = do
        let p = updatePoint upd
        ms <- return . nub . concat . marks =<< getBufferDyn
        ms' <- forM ms $ \m ->do
                 r <- adjMarkRegion m
                 return $ if (updateIsDelete upd && p `nearRegion` r)
                             || p `inRegion` r
                          then Just m
                          else Nothing
        return . catMaybes $ ms'

dependentSiblings :: MarkInfo -> [[MarkInfo]] -> [MarkInfo]
dependentSiblings mark deps =
  case find (elem mark) deps of
    Nothing -> []
    Just lst -> filter (not . (mark==)) lst

updateDependents :: MarkInfo -> BufferM ()
updateDependents m = getBufferDyn >>= updateDependents' m . marks

updateDependents' :: MarkInfo -> [[MarkInfo]] -> BufferM ()
updateDependents' mark deps =
  case dependentSiblings mark deps of
    []   -> return ()
    deps' -> do
      txt <- markText mark
      forM_ deps' $ \d -> do
        dTxt <- markText d
        when (txt /= dTxt) $ setMarkText txt d

markText :: MarkInfo -> BufferM R.YiString
markText m = markRegion m >>= readRegionB

setMarkText :: R.YiString -> MarkInfo -> BufferM ()
setMarkText txt (SimpleMarkInfo _ start) = do
  p <- use $ markPointA start
  c <- readAtB p
  if isSpace c
    then insertNAt txt p
    else do
      r <- regionOfPartNonEmptyAtB unitViWordOnLine Forward p
      modifyRegionB (const txt) r

setMarkText txt mi = do
    start <- use $ markPointA $ startMark mi
    end   <- use $ markPointA $ endMark mi
    let r = mkRegion start end
    modifyRegionB (const txt) r
    when (start == end) $
      markPointA (endMark mi) .= end + Point (R.length txt)

withSimpleRegion :: MarkInfo -> (Region -> BufferM Region) -> BufferM Region
withSimpleRegion (SimpleMarkInfo _ s) f = do
    p <- use $ markPointA s
    c <- readAtB p
    if isSpace c
      then return $ mkRegion p p  -- return empty region
      else f =<< regionOfPartNonEmptyAtB unitViWordOnLine Forward p
withSimpleRegion r _ = error $ "withSimpleRegion: " <> show r

markRegion :: MarkInfo -> BufferM Region
markRegion m@SimpleMarkInfo{} = withSimpleRegion m $ \r -> do
    os <- findOverlappingMarksWith safeMarkRegion concat True r m
    rOs <- mapM safeMarkRegion os
    return . mkRegion (regionStart r) $ foldl' minEnd (regionEnd r) rOs
  where
    minEnd end r = if regionEnd r < end
                   then end
                   else min end $ regionStart r

markRegion m = liftM2 mkRegion
                   (use $ markPointA $ startMark m)
                   (use $ markPointA $ endMark m)

safeMarkRegion :: MarkInfo -> BufferM Region
safeMarkRegion m@(SimpleMarkInfo _ _) = withSimpleRegion m return
safeMarkRegion m = markRegion m

adjMarkRegion :: MarkInfo -> BufferM Region
adjMarkRegion s@(SimpleMarkInfo _ _) = markRegion s

adjMarkRegion m = do
    s <- use $ markPointA $ startMark m
    e <- use $ markPointA $ endMark m
    c <- readAtB e
    when (isWordChar c) $ do adjustEnding e
                             repairOverlappings e
    e' <- use $ markPointA $ endMark m
    s' <- adjustStart s e'
    return $ mkRegion s' e'
  where
    adjustEnding end = do
        r' <- regionOfPartNonEmptyAtB unitViWordOnLine Forward end
        markPointA (endMark m) .= (regionEnd r')

    adjustStart s e = do
        txt <- readRegionB (mkRegion s e)
        let sP = s + (Point . R.length $ R.takeWhile isSpace txt)
        when (sP > s) $
            markPointA (startMark m) .= sP
        return sP

    -- test if we generated overlappings and repair
    repairOverlappings origEnd = do overlappings <- allOverlappingMarks True m
                                    unless (null overlappings) $
                                        markPointA (endMark m) .= origEnd

findOverlappingMarksWith :: (MarkInfo -> BufferM Region)
                         -> ([[MarkInfo]] -> [MarkInfo])
                         -> Bool -> Region -> MarkInfo -> BufferM [MarkInfo]
findOverlappingMarksWith fMarkRegion flattenMarks border r m =
  let markFilter = filter (m /=) . flattenMarks . marks
      regOverlap = fmap (regionsOverlap border r) . fMarkRegion
  in fmap markFilter getBufferDyn >>= filterM regOverlap

findOverlappingMarks :: ([[MarkInfo]] -> [MarkInfo]) -> Bool -> Region ->
                        MarkInfo -> BufferM [MarkInfo]
findOverlappingMarks = findOverlappingMarksWith markRegion

regionsOverlappingMarks :: Bool -> Region -> MarkInfo -> BufferM [MarkInfo]
regionsOverlappingMarks = findOverlappingMarks concat

overlappingMarks :: Bool -> Bool -> MarkInfo -> BufferM [MarkInfo]
overlappingMarks border belongingTogether mark = do
    r <- markRegion mark
    findOverlappingMarks (if belongingTogether
                          then dependentSiblings mark
                          else concat)
                         border
                         r
                         mark

allOverlappingMarks :: Bool -> MarkInfo -> BufferM [MarkInfo]
allOverlappingMarks border = overlappingMarks border False

dependentOverlappingMarks :: Bool -> MarkInfo -> BufferM [MarkInfo]
dependentOverlappingMarks border = overlappingMarks border True

nextBufferMark :: Bool -> BufferM (Maybe MarkInfo)
nextBufferMark deleteLast = do
  BufferMarks ms <- getBufferDyn
  if null ms
    then return Nothing
    else do
      let mks = if deleteLast then const $ tail ms else (tail ms <>)
      putBufferDyn . BufferMarks . mks $ [head ms]
      return . Just $ head ms

isDependentMarker :: (MonadState FBuffer m, Functor m) => Mark -> m Bool
isDependentMarker bMark = do
    DependentMarks ms <- getBufferDyn
    return . elem bMark . concatMap bufferMarkers . concat $ ms

safeDeleteMarkB :: Mark -> BufferM ()
safeDeleteMarkB m = do
    b <- isDependentMarker m
    unless b (deleteMarkB m)

moveToNextBufferMark :: Bool -> BufferM ()
moveToNextBufferMark deleteLast = nextBufferMark deleteLast >>= \case
  Just p  -> mv p
  Nothing -> return ()
  where
    mv (SimpleMarkInfo _ m)   = do
        moveTo =<< use (markPointA m)
        when deleteLast $ safeDeleteMarkB m

    mv (ValuedMarkInfo _ s e) = do
        sp <- use $ markPointA s
        ep <- use $ markPointA e
        deleteRegionB (mkRegion sp ep)
        moveTo sp
        when deleteLast $ do
            safeDeleteMarkB s
            safeDeleteMarkB e

    mv r = error $ "moveToNextBufferMark.mv: " <> show r

-- Keymap support

newtype SupertabExt = Supertab (R.YiString -> Maybe (BufferM ()))

instance Monoid SupertabExt where
  mempty = Supertab $ const Nothing

  (Supertab f) `mappend` (Supertab g) =
    Supertab $ \s -> f s `mplus` g s

superTab :: (MonadInteract m Action Event) => Bool -> SupertabExt -> m ()
superTab caseSensitive (Supertab expander) =
    some (spec KTab ?>>! doSuperTab) >> deprioritize >>! resetComplete
  where
    doSuperTab = do canExpand <- withCurrentBuffer $ do
                                   sol <- atSol
                                   ws  <- hasWhiteSpaceBefore
                                   return $ sol || ws
                    if canExpand
                      then insertTab
                      else runCompleter

    insertTab = withCurrentBuffer $ mapM_ insertB =<< tabB

    runCompleter = do w <- withCurrentBuffer readPrevWordB
                      case expander w of
                        Just cmd -> withCurrentBuffer $ bkillWordB >> cmd
                        _        -> autoComplete

    autoComplete = wordCompleteString' caseSensitive >>=
                   withCurrentBuffer . (bkillWordB >>) . (insertN . R.fromText)

-- | Convert snippet description list into a SuperTab extension
fromSnippets :: Bool -> [(R.YiString, SnippetCmd ())] -> SupertabExt
fromSnippets deleteLast snippets =
  Supertab $ \str -> lookup str $ map (second $ runSnippet deleteLast) snippets

snippet :: MkSnippetCmd a b => a -> SnippetCmd b
snippet = mkSnippetCmd
