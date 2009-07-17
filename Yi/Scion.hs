{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
     FunctionalDependencies, GeneralizedNewtypeDeriving,
     MultiParamTypeClasses, TypeSynonymInstances #-}

module Yi.Scion where

import Yi.Prelude
import Prelude (lines)

import Bag
import Control.Monad
import Data.Maybe
import GHC
import HscTypes
import Outputable (ppr, showSDoc)
import Scion
import Scion.Types hiding (gets) 

import Yi.Buffer
import Yi.Editor
import Yi.Keymap

functionType :: (Int, Int) -> String -> ScionM String
functionType pt fn = do
  addTarget =<< guessTarget fn Nothing

  s <- handleSourceError handleError $ do
    mss  <- modulesInDepOrder
    deps <- Control.Monad.forM mss $ \m -> do
      module' <- loadModule =<< typecheckModule =<< parseModule m
      let t = n . fun_matches . unLoc .
                last . bagToList . last . bagToList . mapBag m' .
                filterBag (\l -> spans (getLoc l) pt) . typecheckedSource
          m' = abs_binds . unLoc
          n (MatchGroup _ t') = t'
      return $ showSDoc $ ppr $ t module'
    return $ deps

  return . last $ s

handleError :: SourceError -> ScionM [String]
handleError err = return [show err]

runScionWithLocation :: ((Int, Int) -> String -> ScionM String) -> YiM ()
runScionWithLocation f = do
  (pt, fn) <- withEditor $ withBuffer0 $ do
          ln  <- curLn
          col <- curCol 
          Just fn  <- gets file
          return ((ln, col), fn)
  s  <- io $ runScion $ f pt fn
  withEditor $ printMsgs $ lines s
