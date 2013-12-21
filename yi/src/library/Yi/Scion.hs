{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
     FunctionalDependencies, GeneralizedNewtypeDeriving,
     MultiParamTypeClasses, TypeSynonymInstances #-}

module Yi.Scion where

import Yi.Prelude
import Yi.Core (msgEditor)
import Prelude (lines)

import Bag
import Data.Maybe
import GHC
import HscTypes
import qualified Outputable as O
import Scion
import Scion.Types
import Scion.Utils
import Outputable
import GHC.SYB.Utils
import PprTyThing (pprTypeForUser)
import FastString (fsLit) -- ghosts

import Yi.Buffer
import Yi.Editor
import Yi.Keymap
import Scion.Inspect ( prettyResult )
import Scion.Inspect.Find ( overlaps, findHsThing, pathToDeepest)
import Scion.Inspect.TypeOf ( typeOf )

loadFile :: String -> ScionM TypecheckedModule
loadFile fn = do
  addTarget =<< guessTarget fn Nothing
  Scion.load LoadAllTargets
  (m:_)  <- modulesInDepOrder
  typecheckModule =<< parseModule m

functionType :: (Int, Int)    -- ^ The line and column of the current point
             -> String        -- ^ The filename in which the point is positioned
             -> ScionM String -- ^ The `ScionM` action resulting in the function's type
functionType pt fn = do
  addTarget =<< guessTarget fn Nothing

  s <- handleSourceError handleError $ do
    mss  <- modulesInDepOrder
    forM mss $ \m -> do
      module' <- loadModule =<< typecheckModule =<< parseModule m
      let t = n . fun_matches . unLoc .
                last . bagToList . last . bagToList . mapBag m' .
                filterBag (\l -> spans (getLoc l) pt) . typecheckedSource
          m' = abs_binds . unLoc
          n (MatchGroup _ t') = t'
      return $ showSDoc $ ppr $ t module'

  return . last $ s
  -- maybe this can be improved by using "thingsAroundPoint"

play :: (Int, Int)    -- ^ The line and column of the current point
             -> String        -- ^ The filename in which the point is positioned
             -> ScionM [String] -- ^ The `ScionM` action resulting in the function's type
play pt fn = do
  addTarget =<< guessTarget fn Nothing

  handleSourceError handleError $ do
    [m]  <- modulesInDepOrder
    module' <- loadModule =<< typecheckModule =<< parseModule m
    lines <$> thingAtPoint pt fn module'


thingsAtPoint :: (Int, Int) -> String -> ScionM String
thingsAtPoint pt fn = do
    addTarget =<< guessTarget fn Nothing
    Scion.load LoadAllTargets
    mss <- modulesInDepOrder
    show <$> forM mss (\ms -> do
      mod <- typecheckModule =<< parseModule ms
      let Just (grp, _, _, _) = renamedSource mod
      let bnds = typecheckedSource mod
      let tyclds = thingsAroundPoint pt $ concat $ hs_tyclds grp
      let ValBindsOut valds _ = hs_valds grp

      return $ showData TypeChecker 2 bnds)

handleError :: SourceError -> ScionM [String]
handleError err = return [show err]

-- This is copied from Protocol.Vim
thingAtPoint :: (TypecheckedMod m) => (Int, Int) -> String -> m -> ScionM String
thingAtPoint (line,col) fname tcm = do
      let loc = srcLocSpan $ mkSrcLoc (fsLit fname) line col
      --let Just (src, _, _, _, _) = renamedSource tcm
      let src = typecheckedSource tcm
      --let in_range = const True
      let in_range = overlaps loc
      let r = findHsThing in_range src
      --return (Just (O.showSDoc (O.ppr $ S.toList r)))
      unqual <- unqualifiedForModule tcm
      case pathToDeepest r of
        Nothing -> return ("no info")
        Just (x,xs) ->
          --return $ Just (O.showSDoc (O.ppr x O.$$ O.ppr xs))
          case typeOf (x,xs) of
            Just t ->
                return $ O.showSDocForUser unqual
                  (prettyResult x O.<+> O.dcolon O.<+>
                    pprTypeForUser True t)
            _ -> return $ O.showSDocDebug (O.ppr x O.$$ O.ppr xs )

runScionWithLocation :: Show a => ((Int, Int) -> String -> ScionM a) -> YiM a
runScionWithLocation f = do
  (pt, fn) <- withEditor $ withBuffer0 $ do
          ln  <- curLn
          col <- curCol
          Just fn  <- gets file
          return ((ln, col), fn)
  io $ runScion $ do
      -- openCabalProject "." "dist" fails ...
      -- loadFile fn
      f pt fn

scion :: YiM ()
scion = msgEditor =<< show <$> runScionWithLocation play
