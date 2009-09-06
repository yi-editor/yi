{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
-- Copyright (C) JP Bernardy 2009

-- | This module defines implementations of syntax-awareness drivers.
 
module Yi.Syntax.Driver where

import Yi.Prelude
import Prelude ()
import Data.List (takeWhile)
import Yi.Syntax hiding (Cache)
import Yi.Syntax.Tree
import Yi.Lexer.Alex (Tok)
import Yi.Region

type Path = [Int]

data Cache state tree tt = Cache {
                                   path :: Path,
                                   cachedStates :: [state],
                                   root, focused :: (tree (Tok tt))
                                 }

mkHighlighter :: forall state tree tt. (IsTree tree, Show state) => 
                 (Scanner Point Char -> Scanner state (tree (Tok tt))) -> 
                     Highlighter (Cache state tree tt) (tree (Tok tt))
mkHighlighter scanner =
  Yi.Syntax.SynHL 
        { hlStartState   = Cache [] [] emptyResult emptyResult
        , hlRun          = updateCache
        , hlGetTree      = root
        , hlFocus        = focus
        }
    where startState :: state
          startState = scanInit    (scanner emptyFileScan)
          emptyResult = scanEmpty (scanner emptyFileScan)
          updateCache :: Scanner Point Char -> Point -> Cache state tree tt -> Cache state tree tt
          updateCache newFileScan dirtyOffset (Cache path cachedStates oldResult _) = Cache path newCachedStates newResult newResult
            where newScan = scanner newFileScan
                  reused :: [state]
                  reused = takeWhile ((< dirtyOffset) . scanLooked (scanner newFileScan)) cachedStates
                  resumeState :: state
                  resumeState = if null reused then startState else last reused
                  
                  newCachedStates = reused ++ fmap fst recomputed
                  recomputed = scanRun newScan resumeState
                  newResult :: tree (Tok tt)
                  newResult = if null recomputed then oldResult else snd $ head $ recomputed
          focus r c@(Cache path states root _focused) = 
              (Cache path' states root $ focused)
              where (path', focused) = fromNodeToFinal r (path,root) 

emptyFileScan :: Scanner Point Char
emptyFileScan = Scanner { scanInit = 0, 
                          scanRun = const [], 
                          scanLooked = id, 
                          scanEmpty = error "emptyFileScan: no scanEmpty" }

