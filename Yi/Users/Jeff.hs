{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
     FunctionalDependencies, GeneralizedNewtypeDeriving,
     MultiParamTypeClasses, TypeSynonymInstances #-}

import Bag
import Control.Monad
import Data.Maybe
import Data.Prototype
import qualified Data.Rope as R
import GHC
import GHC.SYB.Utils
import HscTypes
import Outputable (ppr, showSDoc)
import Scion
import Scion.Types

import Yi.Boot
import Yi.Config
import Yi.Config.Default
import Yi.Core hiding ((.))
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim
import Yi.Misc
import Yi.Search
import Yi.Snippets
import Yi.Style.Library
import Yi.UI.Vty as Vty

main = yi $ defaultVimConfig
  { defaultKm = myVimKeymap
  , configUI = (configUI defaultVimConfig)
    { configTheme = defaultLightTheme 
    , configWindowFill = '~'
    }
  , startActions = [makeAction (maxStatusHeightA %= 40 :: EditorM ())]
  , startFrontEnd = Vty.start
  }

deleteSnippetMarkers = True

myVimKeymap = mkKeymap $ defKeymap `override` \super self -> super
  { v_top_level = v_top_level super ||>
      (char ';' ?>>! resetRegexE)

  , v_ins_char  = (v_ins_char super) <|>
      choice [ ctrlCh 's' ?>>! moveToNextBufferMark deleteSnippetMarkers
             , meta (spec KLeft)  ?>>! prevWordB
             , meta (spec KRight) ?>>! nextWordB
             ]
  , v_ex_cmds = exCmds [("scion", runScionStuff, Nothing)]
  }

scionAction :: (Int, Int) -> String -> ScionM String
scionAction pt fn = do
  addTarget =<< guessTarget fn Nothing

  s <- handleSourceError handleError $ do
    mss  <- modulesInDepOrder
    deps <- Control.Monad.forM mss $ \m ->do
      module' <- loadModule =<< typecheckModule =<< parseModule m
      let t = n . fun_matches . unLoc .
                last . bagToList . last . bagToList . mapBag m .
                filterBag (\l -> spans (getLoc l) pt) . typecheckedSource
          m = abs_binds . unLoc
          n (MatchGroup _ t) = t
      return $ showSDoc $ ppr $ t module'
    return $ deps

  return . last $ s

handleError :: SourceError -> ScionM [String]
handleError error = return [show error]

runScionStuff :: String -> YiM ()
runScionStuff fn = do
  (pt, fn) <- withEditor $ withBuffer0 $ do
          ln  <- curLn
          col <- curCol 
          fn  <- Yi.Core.gets $ shortIdentString []
          return ((ln, col), fn)
  s  <- io $ runScion $ scionAction pt fn
  withEditor $ printMsgs $ lines s
