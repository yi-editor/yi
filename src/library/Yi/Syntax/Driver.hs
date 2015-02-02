{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | This module defines implementations of syntax-awareness drivers.

module Yi.Syntax.Driver where

import           Data.Map        (Map)
import qualified Data.Map        as M (Map, assocs, empty, findWithDefault, fromList)
import           Yi.Buffer.Basic (WindowRef)
import           Yi.Lexer.Alex   (Tok)
import           Yi.Syntax       hiding (Cache)
import           Yi.Syntax.Tree  (IsTree, fromNodeToFinal)

type Path = [Int]

data Cache state tree tt = Cache {
                                   path :: M.Map WindowRef Path,
                                   cachedStates :: [state],
                                   root :: tree (Tok tt),
                                   focused :: !(M.Map WindowRef (tree (Tok tt)))
                                 }

mkHighlighter :: forall state tree tt. (IsTree tree, Show state) =>
                 (Scanner Point Char -> Scanner state (tree (Tok tt))) ->
                     Highlighter (Cache state tree tt) (tree (Tok tt))
mkHighlighter scanner =
  Yi.Syntax.SynHL
        { hlStartState   = Cache M.empty [] emptyResult M.empty
        , hlRun          = updateCache
        , hlGetTree      = \(Cache _ _ _ focused) w -> M.findWithDefault emptyResult w focused
        , hlFocus        = focus
        }
    where startState :: state
          startState = scanInit    (scanner emptyFileScan)
          emptyResult = scanEmpty (scanner emptyFileScan)
          updateCache :: Scanner Point Char -> Point -> Cache state tree tt -> Cache state tree tt
          updateCache newFileScan dirtyOffset (Cache path cachedStates oldResult _) = Cache path newCachedStates newResult M.empty
            where newScan = scanner newFileScan
                  reused :: [state]
                  reused = takeWhile ((< dirtyOffset) . scanLooked (scanner newFileScan)) cachedStates
                  resumeState :: state
                  resumeState = if null reused then startState else last reused

                  newCachedStates = reused ++ fmap fst recomputed
                  recomputed = scanRun newScan resumeState
                  newResult :: tree (Tok tt)
                  newResult = if null recomputed then oldResult else snd $ head recomputed
          focus r (Cache path states root _focused) =
              Cache path' states root focused
              where (path', focused) = unzipFM $ zipWithFM (\newpath oldpath -> fromNodeToFinal newpath (oldpath,root)) [] r path

unzipFM :: Ord k => [(k,(u,v))] -> (Map k u, Map k v)
unzipFM l = (M.fromList mu, M.fromList mv)
    where (mu, mv) = unzip [((k,u),(k,v)) | (k,(u,v)) <- l]

zipWithFM :: Ord k => (u -> v -> w) -> v -> Map k u -> Map k v -> [(k,w)]
zipWithFM f v0 mu mv = [ (k,f u (M.findWithDefault v0 k mv) ) | (k,u) <- M.assocs mu]
