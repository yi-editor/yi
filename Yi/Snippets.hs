{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    NoMonomorphismRestriction, TypeSynonymInstances #-}
module Yi.Snippets where

import Prelude ()
import Yi.Prelude

import Control.Arrow
import Control.Monad.RWS hiding (mapM_)
import Data.List hiding (foldl)

import Yi.Buffer
import Yi.Dynamic
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Keymap.Vim (savingInsertCharB)
import Yi.TextCompletion

type SnippetCmd = RWST (Int, Int) [MarkInfo] () BufferM

data SnippetMark = SimpleMark !Int
                 | ValuedMark !Int String

data MarkInfo = SimpleMarkInfo { userIndex :: !Int, startMark :: !Mark }
              | ValuedMarkInfo { userIndex :: !Int, startMark :: !Mark, endMark :: !Mark } 
  deriving (Eq, Show)
  
newtype BufferMarks = BufferMarks { bufferMarks :: [MarkInfo] }
  deriving (Eq, Show, Monoid, Typeable)
  
instance Initializable BufferMarks where
  initial = BufferMarks []

instance Ord MarkInfo where
  a `compare` b = (userIndex a) `compare` (userIndex b)

cursor = SimpleMark
cursorWith = ValuedMark

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
        let newMarks = sort markInfo
        modA bufferDynamicValueA ((BufferMarks newMarks) `mappend`)
        moveToNextBufferMark deleteLast
    return a
    
nextBufferMark :: Bool -> BufferM (Maybe MarkInfo)
nextBufferMark deleteLast = do
    BufferMarks ms <- getA bufferDynamicValueA
    if (null ms) 
      then return Nothing
      else do putA bufferDynamicValueA . BufferMarks . (if deleteLast then (const $ tail ms) else (tail ms ++)) $ [head ms]
              return . Just $ head ms

moveToNextBufferMark :: Bool -> BufferM ()
moveToNextBufferMark deleteLast = do
    p <- nextBufferMark deleteLast
    case p of
      Just p  -> mv p
      Nothing -> return ()
  where
    mv (SimpleMarkInfo _ m)   = do
        moveTo =<< getMarkPointB m
        when deleteLast $ deleteMarkB m
            
    mv (ValuedMarkInfo _ s e) = do
        sp <- getMarkPointB s
        ep <- getMarkPointB e
        deleteRegionB (mkRegion sp ep)
        moveTo sp
        when deleteLast $ do
            deleteMarkB s
            deleteMarkB e

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
