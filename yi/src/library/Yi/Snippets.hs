{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    NoMonomorphismRestriction, TypeSynonymInstances, TemplateHaskell, CPP,
    StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-incomplete-patterns -fno-warn-name-shadowing #-}
module Yi.Snippets where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.RWS hiding (mapM, mapM_, sequence, get, put)
import Control.Lens hiding (Action)
import Data.Binary
import Data.Typeable
import Data.Foldable (find)
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif
import Data.List hiding (find, elem, concat, concatMap)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Default

import Yi.Buffer
import Yi.Dynamic
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.TextCompletion

type SnippetCmd = RWST (Int, Int) [MarkInfo] () BufferM

data SnippetMark = SimpleMark !Int
                 | ValuedMark !Int String
                 | DependentMark !Int

data MarkInfo = SimpleMarkInfo { userIndex :: !Int, startMark :: !Mark }
              | ValuedMarkInfo { userIndex :: !Int, startMark :: !Mark, endMark :: !Mark }
              | DependentMarkInfo { userIndex :: !Int, startMark :: !Mark, endMark :: !Mark }
  deriving (Eq, Show)

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''MarkInfo)
#else
deriving instance Generic MarkInfo
instance Binary MarkInfo
#endif

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

cursor = SimpleMark
cursorWith = ValuedMark
dep = DependentMark

isDependentMark (SimpleMarkInfo{})    = False
isDependentMark (ValuedMarkInfo{})    = False
isDependentMark (DependentMarkInfo{}) = True

bufferMarkers (SimpleMarkInfo _ s) = [s]
bufferMarkers m = [startMark m, endMark m]

-- used to translate a datatype into a snippet cmd for
-- freely combining data with '&'
class MkSnippetCmd a b | a -> b where
  mkSnippetCmd :: a -> SnippetCmd b

instance MkSnippetCmd String () where
  mkSnippetCmd = text

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
mkMark = lift $ do p <- pointB
                   newMarkB $ MarkValue p Backward

-- Indentation support has been temporarily removed
text :: String -> SnippetCmd ()
text txt = do
    (_, indent) <- ask
    indentSettings <- lift indentSettingsB
    lift . foldl' (>>) (return ()) .
        intersperse (newlineB >> indentToB indent) .
        map (if expandTabs indentSettings
             then insertN . expand indentSettings ""
             else insertN) $ lines' txt
  where
    lines' txt = if last txt == '\n' -- TODO: not very efficient yet
                  then lines txt ++ [""]
                  else lines txt

    expand _ str [] = reverse str
    expand indentSettings str (s:rst)
        | s == '\t' = expand indentSettings (replicate (tabSize indentSettings) ' ' ++ str) rst
        | otherwise = expand indentSettings (s:str) rst

-- unfortunatelly data converted to snippets are no monads,
-- but & is very similar to >> abd &> is similar to >>=,
-- since SnippetCmd's can be used monadic
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
        (%=) bufferDynamicValueA (BufferMarks newMarks `mappend`)
        unless (null newDepMarks) $
            (%=) bufferDynamicValueA (DependentMarks newDepMarks `mappend`)
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
findEditedMarks upds = liftM (nub . concat) (mapM findEditedMarks' upds)
  where
    findEditedMarks' :: Update -> BufferM [MarkInfo]
    findEditedMarks' upd = do
        let p = updatePoint upd
        ms <- return . nub . concat . marks =<< use bufferDynamicValueA
        ms <- forM ms $ \m ->do
                r <- adjMarkRegion m
                return $ if (updateIsDelete upd && p `nearRegion` r)
                            || p `inRegion` r
                         then Just m
                         else Nothing
        return . catMaybes $ ms

dependentSiblings :: MarkInfo -> [[MarkInfo]] -> [MarkInfo]
dependentSiblings mark deps =
  case find (elem mark) deps of
    Nothing -> []
    Just lst -> filter (not . (mark==)) lst

updateDependents :: MarkInfo -> BufferM ()
updateDependents m = use bufferDynamicValueA >>= updateDependents' m . marks

updateDependents' :: MarkInfo -> [[MarkInfo]] -> BufferM ()
updateDependents' mark deps =
    case dependentSiblings mark deps of
      []   -> return ()
      deps -> do
          txt <- markText mark
          forM_ deps $ \d -> do
              dTxt <- markText d
              when (txt /= dTxt) $
                  setMarkText txt d

markText :: MarkInfo -> BufferM String
markText m = markRegion m >>= readRegionB

setMarkText :: String -> MarkInfo -> BufferM ()
setMarkText txt (SimpleMarkInfo _ start) = do
    p <- use $ markPointA start
    c <- readAtB p
    if isSpace c
      then insertNAt txt p
      else do  r <- regionOfPartNonEmptyAtB unitViWordOnLine Forward p
               modifyRegionClever (const txt) r

setMarkText txt mi = do
    start <- use $ markPointA $ startMark mi
    end   <- use $ markPointA $ endMark mi
    let r = mkRegion start end
    modifyRegionClever (const txt) r
    when (start == end) $
        markPointA (endMark mi) .= (end + (Point $ length txt))

withSimpleRegion (SimpleMarkInfo _ s) f = do
    p <- use $ markPointA s
    c <- readAtB p
    if isSpace c
      then return $ mkRegion p p  -- return empty region
      else f =<< regionOfPartNonEmptyAtB unitViWordOnLine Forward p

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

safeMarkRegion m@(SimpleMarkInfo _ _) = withSimpleRegion m return
safeMarkRegion m = markRegion m

adjMarkRegion s@(SimpleMarkInfo _ _) = markRegion s

adjMarkRegion m = do
    s <- use $ markPointA $ startMark m
    e <- use $ markPointA $ endMark m
    c <- readAtB e
    when (isWordChar c) $ do adjustEnding e
                             repairOverlappings e
    e <- use $ markPointA $ endMark m
    s <- adjustStart s e
    return $ mkRegion s e
  where
    adjustEnding end = do
        r' <- regionOfPartNonEmptyAtB unitViWordOnLine Forward end
        markPointA (endMark m) .= (regionEnd r')

    adjustStart s e = do
        txt <- readRegionB (mkRegion s e)
        let sP = s + (Point . length $ takeWhile isSpace txt)
        when (sP > s) $
            markPointA (startMark m) .= sP
        return sP

    -- test if we generated overlappings and repair
    repairOverlappings origEnd = do overlappings <- allOverlappingMarks True m
                                    unless (null overlappings) $
                                        markPointA (endMark m) .= origEnd

findOverlappingMarksWith :: (MarkInfo -> BufferM Region) ->
                            ([[MarkInfo]] -> [MarkInfo]) -> Bool -> Region ->
                            MarkInfo -> BufferM [MarkInfo]
findOverlappingMarksWith fMarkRegion flattenMarks border r m =
    liftM (filter (not . (m ==)) . flattenMarks . marks) (use bufferDynamicValueA) >>=
    filterM (liftM (regionsOverlap border r) . fMarkRegion)

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
    BufferMarks ms <- use bufferDynamicValueA
    if null ms
      then return Nothing
      else do assign bufferDynamicValueA . BufferMarks . (if deleteLast then const $ tail ms else (tail ms ++)) $ [head ms]
              return . Just $ head ms

isDependentMarker bMark = do
    DependentMarks ms <- use bufferDynamicValueA
    return . elem bMark . concatMap bufferMarkers . concat $ ms

safeDeleteMarkB m = do
    b <- isDependentMarker m
    unless b (deleteMarkB m)

moveToNextBufferMark :: Bool -> BufferM ()
moveToNextBufferMark deleteLast = do
    p <- nextBufferMark deleteLast
    case p of
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

-- Keymap support

newtype SupertabExt = Supertab (String -> Maybe (BufferM ()))

instance Monoid SupertabExt where
  mempty = Supertab $ const Nothing

  (Supertab f) `mappend` (Supertab g) =
    Supertab $ \s -> f s `mplus` g s

superTab :: (MonadInteract m Action Event) => Bool -> SupertabExt -> m ()
superTab caseSensitive (Supertab expander) =
    some (spec KTab ?>>! doSuperTab) >> deprioritize >>! resetComplete
  where
    doSuperTab = do canExpand <- withBuffer $ do
                                   sol <- atSol
                                   ws  <- hasWhiteSpaceBefore
                                   return $ sol || ws
                    if canExpand
                      then insertTab
                      else runCompleter

    insertTab = withBuffer $ mapM_ insertB =<< tabB

    runCompleter = do w <- withBuffer readPrevWordB
                      case expander w of
                        Just cmd -> withBuffer $ bkillWordB >> cmd
                        _        -> autoComplete

    autoComplete = wordCompleteString' caseSensitive >>=
                   withBuffer . (bkillWordB >>) . insertN

-- | Convert snippet description list into a SuperTab extension
fromSnippets :: Bool -> [(String, SnippetCmd ())] -> SupertabExt
fromSnippets deleteLast snippets =
  Supertab $ \str -> lookup str $ map (second $ runSnippet deleteLast) snippets

snippet = mkSnippetCmd
