{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    NoMonomorphismRestriction, TypeSynonymInstances #-}
module Yi.Snippets where

import Prelude ()
import Yi.Prelude

import Control.Arrow
import Control.Monad.RWS hiding (mapM_, forM, forM_, sequence)
import Data.List hiding (foldl, find, elem, concat, concatMap)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isJust)

import Yi.Buffer
import Yi.Dynamic
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Keymap.Vim (savingInsertCharB)
import Yi.TextCompletion

type SnippetCmd = RWST (Int, Int) [MarkInfo] () BufferM

data SnippetMark = SimpleMark !Int
                 | ValuedMark !Int String
                 | DependentMark !Int

data MarkInfo = SimpleMarkInfo { userIndex :: !Int, startMark :: !Mark }
              | ValuedMarkInfo { userIndex :: !Int, startMark :: !Mark, endMark :: !Mark } 
              | DependentMarkInfo { userIndex :: !Int, startMark :: !Mark, endMark :: !Mark }
  deriving (Eq, Show)
  
newtype BufferMarks = BufferMarks { bufferMarks :: [MarkInfo] }
  deriving (Eq, Show, Monoid, Typeable)
  
newtype DependentMarks = DependentMarks { marks :: [[MarkInfo]] }
  deriving (Eq, Show, Monoid, Typeable)
  
instance Initializable BufferMarks where
  initial = BufferMarks []
  
instance Initializable DependentMarks where
  initial = DependentMarks []

instance Ord MarkInfo where
  a `compare` b = (userIndex a) `compare` (userIndex b)

cursor = SimpleMark
cursorWith = ValuedMark
dep = DependentMark

isDependentMark (SimpleMarkInfo _ _)    = False
isDependentMark (ValuedMarkInfo _ _ _)  = False
isDependentMark (DependentMarkInfo _ _ _) = True

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
    lift . foldl (>>) (return ()) . 
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
        | s == '\t' = expand indentSettings ((replicate (tabSize indentSettings) ' ') ++ str) rst
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
        modA bufferDynamicValueA ((BufferMarks newMarks) `mappend`)
        unless (null newDepMarks) $ do
            modA bufferDynamicValueA ((DependentMarks newDepMarks) `mappend`)
        moveToNextBufferMark deleteLast
    return a
  where
    len1 (x:[]) = True
    len1 _      = False
    
    belongTogether a b = userIndex a == userIndex b
    
updateUpdatedMarks :: [Update] -> BufferM ()
updateUpdatedMarks upds = findEditedMarks upds >>=
                          mapM_ updateDependents
    
findEditedMarks :: [Update] -> BufferM [MarkInfo]
findEditedMarks upds = sequence (map findEditedMarks' upds) >>=
                       return . nub . concat
  where 
    findEditedMarks' :: Update -> BufferM [MarkInfo]
    findEditedMarks' upd = do
        let p = updatePoint upd
        ms <- return . nub . concat . marks =<< getA bufferDynamicValueA
        ms <- forM ms $ \m ->do 
                r <- markRegion m
                return $ if (updateIsDelete upd && p `nearRegion` r) 
                            || p `inRegion` r
                         then Just m
                         else Nothing
                
                                {-
                                if p `inRegion` r
                                         then Just m
                                         else Nothing
                                -}                                
        return . map fromJust . filter isJust $ ms

updateDependents :: MarkInfo -> BufferM ()
updateDependents m = getA bufferDynamicValueA >>= updateDependents' m . marks
    
updateDependents' :: MarkInfo -> [[MarkInfo]] -> BufferM ()
updateDependents' mark deps =
    case depSiblings of
      Nothing -> return ()
      Just deps -> do 
          txt <- markText mark
          forM_ deps $ \d -> do
              dTxt <- markText d
              when (txt /= dTxt) $
                  setMarkText txt d
  where
    depSiblings = case find (elem mark) deps of
                    Nothing  -> Nothing
                    Just lst -> case filter (not . (mark==)) lst of
                                  [] -> Nothing
                                  l  -> Just l
                                  
markText :: MarkInfo -> BufferM String
markText (SimpleMarkInfo _ start) = do
    p <- getMarkPointB start
    c <- readAtB p
    if isSpace c
      then return ""
      else readRegionB =<< regionOfPartNonEmptyB unitViWordOnLine Forward
      
markText m = markRegion m >>= readRegionB

setMarkText :: String -> MarkInfo -> BufferM ()
setMarkText txt (SimpleMarkInfo _ start) = do
    p <- getMarkPointB start
    c <- readAtB p
    if (isSpace c)
      then insertNAt txt p
      else do  old_p <- pointB
               moveTo p
               r <- regionOfPartNonEmptyB unitViWordOnLine Forward
               moveTo old_p
               modifyRegionClever (const txt) r

setMarkText txt mi = do
    start <- getMarkPointB $ startMark mi
    end   <- getMarkPointB $  endMark mi
    let r = mkRegion start end
    modifyRegionClever (const txt) r
    when (start == end) $
        setMarkPointB (endMark mi) (end + (Point $ length txt))

markRegion (SimpleMarkInfo _ s) = do
    p <- getMarkPointB s
    c <- readAtB p
    if isSpace c
      then return $ mkRegion p p
      else regionOfPartNonEmptyB unitViWordOnLine Forward

markRegion (ValuedMarkInfo _ s e) = getMarkRegion s e
markRegion (DependentMarkInfo _ s e) = getMarkRegion s e

getMarkRegion start end = do
    s <- getMarkPointB start
    e <- getMarkPointB end
    c <- readAtB e
    if not $ isWordChar c
      then return $ mkRegion s e
      else do oldP <- pointB
              moveTo e
              r' <- regionOfPartNonEmptyB unitViWordOnLine Forward
              setMarkPointB end (regionEnd r')
              moveTo oldP
              return $ mkRegion s (regionEnd r')

nextBufferMark :: Bool -> BufferM (Maybe MarkInfo)
nextBufferMark deleteLast = do
    BufferMarks ms <- getA bufferDynamicValueA
    if (null ms) 
      then return Nothing
      else do putA bufferDynamicValueA . BufferMarks . (if deleteLast then (const $ tail ms) else (tail ms ++)) $ [head ms]
              return . Just $ head ms
              
isDependentMarker bMark = do
    DependentMarks ms <- getA bufferDynamicValueA
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
        moveTo =<< getMarkPointB m
        when deleteLast $ safeDeleteMarkB m
            
    mv (ValuedMarkInfo _ s e) = do
        sp <- getMarkPointB s
        ep <- getMarkPointB e
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

    insertTab = withBuffer $ mapM_ savingInsertCharB =<< tabB

    runCompleter = do w <- withBuffer $ readPrevWordB
                      case expander w of
                        Just cmd -> withBuffer $ do bkillWordB >> cmd
                        _        -> autoComplete

    autoComplete = wordCompleteString' caseSensitive >>=
                   withBuffer . (bkillWordB >>) . insertN

-- | Convert snippet description list into a SuperTab extension
fromSnippets :: Bool -> [(String, SnippetCmd ())] -> SupertabExt
fromSnippets deleteLast snippets =
  Supertab $ \str -> lookup str $ map (second $ runSnippet deleteLast) snippets

snippet = mkSnippetCmd
