{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
#ifdef HINT
{-# LANGUAGE FlexibleContexts #-}
#endif
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Eval
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Evaluator for actions ('Action', 'YiAction'). Uses a @GHCi@ session
-- under the hood.

module Yi.Eval (
        -- * Main (generic) evaluation interface
        execEditorAction,
        getAllNamesInScope,
        describeNamedAction,
        Evaluator(..),
        evaluator,
        -- ** Standard evaluators
#ifdef HINT
        ghciEvaluator,
#endif
        publishedActionsEvaluator,
        publishedActions,
        publishAction,
        -- * Eval/Interpretation
        jumpToErrorE,
        jumpToE,
        consoleKeymap
) where

import Prelude hiding (mapM_)

import Control.Applicative ( (<$>), (<*>) )
import Control.Lens ( (^.), (<&>), (.=), (%=) )
import Control.Monad (when)
import Data.Array ( elems )
import Data.Binary ( Binary )
import Data.Default ( Default, def )
import Data.Foldable ( mapM_ )
import qualified Data.HashMap.Strict as M
    ( HashMap, insert, lookup, empty, keys )
import Data.Monoid ( mempty, Monoid, (<>) )
import Data.Typeable ( Typeable )
#ifdef HINT
import Control.Concurrent
    ( takeMVar, putMVar, newEmptyMVar, MVar, forkIO )
import Control.Monad
    ( Monad((>>), (>>=), return), void, forever )
import Control.Monad.Base ( MonadBase )
import Control.Monad.Catch ( try )
import Control.Monad.Trans ( lift )
import Data.Binary ( get, put )
import Data.List ( sort )
import qualified Language.Haskell.Interpreter as LHI
    ( typeOf,
      setImportsQ,
      searchPath,
      set,
      runInterpreter,
      ModuleElem(Data, Class, Fun),
      getModuleExports,
      as,
      loadModules,
      languageExtensions,
      OptionVal((:=)),
      InterpreterError,
      Extension(OverloadedStrings),
      setTopLevelModules,
      interpret )
import System.Directory ( doesFileExist )
import Yi.Boot.Internal ( reload )
import Yi.Core ( errorEditor )
import Yi.Editor
    ( getEditorDyn,
      putEditorDyn,
      MonadEditor)
import qualified Yi.Paths ( getEvaluatorContextFilename )
import Yi.String ( showT )
import Yi.Utils ( io )
#endif
import Text.Read ( readMaybe )
import Yi.Buffer
    ( gotoLn,
      moveXorEol,
      BufferM,
      readLnB,
      pointB,
      botB,
      insertN,
      getBookmarkB,
      markPointA )
import Yi.Config.Simple.Types ( customVariable, Field, ConfigM )
import Yi.Core ( runAction )
import Yi.Types ( YiVariable, YiConfigVariable )
import Yi.Editor
    ( printMsg,
      askCfg,
      withCurrentBuffer,
      withCurrentBuffer )
import Yi.File ( openingNewFile )
import Yi.Hooks ( runHook )
import Yi.Keymap
    ( YiM, Action, YiAction, makeAction, Keymap, write )
import Yi.Keymap.Keys ( event, Event(..), Key(KEnter) )
import Yi.Regex ( Regex, makeRegex, matchOnceText )
import qualified Yi.Rope as R
    ( toString, YiString, splitAt, length )
import Yi.Utils ( makeLensesWithSuffix )


-- TODO: should we be sticking Text here?

-- | Runs the action, as written by the user.
--
-- The behaviour of this function can be customised by modifying the
-- 'Evaluator' variable.
execEditorAction :: String -> YiM ()
execEditorAction = runHook execEditorActionImpl

-- | Lists the action names in scope, for use by 'execEditorAction',
-- and 'help' index.
--
-- The behaviour of this function can be customised by modifying the
-- 'Evaluator' variable.
getAllNamesInScope :: YiM [String]
getAllNamesInScope = runHook getAllNamesInScopeImpl

-- | Describes the named action in scope, for use by 'help'.
--
-- The behaviour of this function can be customised by modifying the
-- 'Evaluator' variable.
describeNamedAction :: String -> YiM String
describeNamedAction = runHook describeNamedActionImpl

-- | Config variable for customising the behaviour of
-- 'execEditorAction' and 'getAllNamesInScope'.
--
-- Set this variable using 'evaluator'. See 'ghciEvaluator' and
-- 'finiteListEvaluator' for two implementation.
data Evaluator = Evaluator
  { execEditorActionImpl :: String -> YiM ()
    -- ^ implementation of 'execEditorAction'
  , getAllNamesInScopeImpl :: YiM [String]
    -- ^ implementation of 'getAllNamesInScope'
  , describeNamedActionImpl :: String -> YiM String
    -- ^ describe named action (or at least its type.), simplest implementation is at least @return@.
  } deriving (Typeable)

-- | The evaluator to use for 'execEditorAction' and
-- 'getAllNamesInScope'.
evaluator :: Field Evaluator
evaluator = customVariable

-- * Evaluator based on GHCi
-- | Cached variable for getAllNamesInScopeImpl
newtype NamesCache = NamesCache [String] deriving (Typeable, Binary)

instance Default NamesCache where
    def = NamesCache []
instance YiVariable NamesCache

-- | Cached dictioary for describeNameImpl
newtype HelpCache = HelpCache (M.HashMap String String) deriving (Typeable, Binary)

instance Default HelpCache where
    def = HelpCache M.empty
instance YiVariable HelpCache

#ifdef HINT
type HintRequest = (String, MVar (Either LHI.InterpreterError Action))
newtype HintThreadVar = HintThreadVar (Maybe (MVar HintRequest))
  deriving (Typeable, Default)

instance Binary HintThreadVar where
  put _ = return ()
  get = return def
instance YiVariable HintThreadVar

getHintThread :: (MonadEditor m, MonadBase IO m) => m (MVar HintRequest)
getHintThread = do
  HintThreadVar x <- getEditorDyn
  case x of
    Just t -> return t
    Nothing -> do
      req <- io newEmptyMVar
      contextFile <- Yi.Paths.getEvaluatorContextFilename
      void . io . forkIO $ hintEvaluatorThread req contextFile
      putEditorDyn . HintThreadVar $ Just req
      return req

hintEvaluatorThread :: MVar HintRequest -> FilePath -> IO ()
hintEvaluatorThread request contextFile = do
  haveUserContext <- doesFileExist contextFile
  void $ LHI.runInterpreter $ do
    LHI.set [LHI.searchPath LHI.:= []]

    LHI.set [LHI.languageExtensions LHI.:= [ LHI.OverloadedStrings ]]
    when haveUserContext $ do
      LHI.loadModules [contextFile]
      LHI.setTopLevelModules ["Env"]

    -- Yi.Keymap: Action lives there
    LHI.setImportsQ [("Yi", Nothing), ("Yi.Keymap",Just "Yi.Keymap")]
    forever $ do
      (s,response) <- lift $ takeMVar request
      res <- try $ LHI.interpret ("Yi.makeAction (" ++ s ++ ")") (LHI.as :: Action)
      lift $ putMVar response res

-- Evaluator implemented by calling GHCi. This evaluator can run
-- arbitrary expressions in the class 'YiAction'.
--
-- The following two imports are always present:
--
-- > import Yi
-- > import qualified Yi.Keymap as Yi.Keymap
--
-- Also, if the file
--
-- > $HOME/.config/yi/local/Env.hs
--
-- exists, it is imported unqualified.
ghciEvaluator :: Evaluator
ghciEvaluator = Evaluator { execEditorActionImpl = execAction
                          , getAllNamesInScopeImpl = getNames
                          , describeNamedActionImpl = describeName -- TODO: use haddock to add docs
                          }
  where
    execAction :: String -> YiM ()
    execAction "reload" = reload
    execAction s = do
      request <- getHintThread
      res <- io $ do
        response <- newEmptyMVar
        putMVar request (s,response)
        takeMVar response
      case res of
        Left err -> errorEditor (showT err)
        Right action -> runAction action

    getNames :: YiM [String]
    getNames = do
      NamesCache cache <- getEditorDyn
      result <- if null cache
                then do
                  res <- io $ LHI.runInterpreter $ do
                    LHI.set [LHI.searchPath LHI.:= []]
                    LHI.getModuleExports "Yi"
                  return $ case res of
                    Left err ->[show err]
                    Right exports -> flattenExports exports
                else return $ sort cache
      putEditorDyn $ NamesCache result
      return result

    flattenExports :: [LHI.ModuleElem] -> [String]
    flattenExports = concatMap flattenExport

    flattenExport :: LHI.ModuleElem -> [String]
    flattenExport (LHI.Fun x) = [x]
    flattenExport (LHI.Class _ xs) = xs
    flattenExport (LHI.Data _ xs) = xs
      
    describeName :: String -> YiM String
    describeName name = do
      HelpCache cache <- getEditorDyn
      description <- case name `M.lookup` cache of
                       Nothing -> do
                         result <- io $ LHI.runInterpreter $ do
                           LHI.set [LHI.searchPath LHI.:= []]
                           -- when haveUserContext $ do
                           --   LHI.loadModules [contextFile]
                           --   LHI.setTopLevelModules ["Env"]
                           LHI.setImportsQ [("Yi", Nothing), ("Yi.Keymap",Just "Yi.Keymap")]
                           LHI.typeOf name
                         let newDescription = either show id result
                         putEditorDyn $ HelpCache $ M.insert name newDescription cache
                         return newDescription
                       Just description -> return description
      return $ name ++ " :: " ++ description

#endif

-- * 'PublishedActions' evaluator

newtype PublishedActions = PublishedActions {
    _publishedActions :: M.HashMap String Action
  } deriving(Typeable, Monoid)

instance Default PublishedActions where def = mempty

makeLensesWithSuffix "A" ''PublishedActions
instance YiConfigVariable PublishedActions

-- | Accessor for the published actions. Consider using
-- 'publishAction'.
publishedActions :: Field (M.HashMap String Action)
publishedActions = customVariable . _publishedActionsA

-- | Publish the given action, by the given name. This will overwrite
-- any existing actions by the same name.
publishAction :: (YiAction a x, Show x) => String -> a -> ConfigM ()
publishAction s a = publishedActions %= M.insert s (makeAction a)

-- | Evaluator based on a fixed list of published actions. Has a few
-- differences from 'ghciEvaluator':
--
-- * expressions can't be evaluated
--
-- * all suggested actions are actually valued
--
-- * (related to the above) doesn't contain junk actions from Prelude
--
-- * doesn't require GHCi backend, so uses less memory
publishedActionsEvaluator :: Evaluator
publishedActionsEvaluator = Evaluator
  { getAllNamesInScopeImpl = askCfg <&> M.keys . (^. publishedActions)
  , execEditorActionImpl = \s ->
      askCfg <&> M.lookup s . (^. publishedActions) >>= mapM_ runAction
  , describeNamedActionImpl = return -- TODO: try to show types using TemplateHaskell!
  }

-- * Miscellaneous interpreter

-- | Jumps to specified position in a given file.
jumpToE :: FilePath -- ^ Filename to make the jump in.
        -> Int -- ^ Line to jump to.
        -> Int -- ^ Column to jump to.
        -> YiM ()
jumpToE filename line column =
  openingNewFile filename $ gotoLn line >> moveXorEol column

-- | Regex parsing the error message format.
errorRegex :: Regex
errorRegex = makeRegex ("^(.+):([0-9]+):([0-9]+):.*$" :: String)

-- | Parses an error message. Fails if it can't parse out the needed
-- information, namely filename, line number and column number.
parseErrorMessage :: R.YiString -> Maybe (String, Int, Int)
parseErrorMessage ln = do
  (_ ,result, _) <- matchOnceText errorRegex (R.toString ln)
  case take 3 $ map fst $ elems result of
    [_, fname, l, c] -> (,,) <$> return fname <*> readMaybe l <*> readMaybe c
    _                        -> Nothing

-- | Tries to parse an error message at current line using
-- 'parseErrorMessage'.
parseErrorMessageB :: BufferM (Maybe (String, Int, Int))
parseErrorMessageB = parseErrorMessage <$> readLnB

-- | Tries to jump to error at the current line. See
-- 'parseErrorMessageB'.
jumpToErrorE :: YiM ()
jumpToErrorE = withCurrentBuffer parseErrorMessageB >>= \case
  Nothing -> printMsg "Couldn't parse out an error message."
  Just (f, l, c) -> jumpToE f l c

prompt :: R.YiString
prompt = "Yi> "

-- | Tries to strip the 'prompt' from the front of the given 'String'.
-- If the prompt is not found, returns the input command as-is.
takeCommand :: R.YiString -> R.YiString
takeCommand t = case R.splitAt (R.length prompt) t of
  (f, s) -> if f == prompt then s else t

consoleKeymap :: Keymap
consoleKeymap = do
  _ <- event (Event KEnter [])
  write $ withCurrentBuffer readLnB >>= \x -> case parseErrorMessage x of
    Just (f,l,c) -> jumpToE f l c
    Nothing -> do
      withCurrentBuffer $ do
        p <- pointB
        botB
        p' <- pointB
        when (p /= p') $ insertN ("\n" <> prompt <> takeCommand x)
        insertN "\n"
        pt <- pointB
        insertN prompt
        bm <- getBookmarkB "errorInsert"
        markPointA bm .= pt
      execEditorAction . R.toString $ takeCommand x

instance Default Evaluator where
#ifdef HINT
    def = ghciEvaluator
#else
    def = publishedActionsEvaluator
#endif

instance YiConfigVariable Evaluator