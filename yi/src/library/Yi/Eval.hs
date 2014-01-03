{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, RecordWildCards #-}

module Yi.Eval (
        -- * Main (generic) evaluation interface
        execEditorAction,
        getAllNamesInScope,
        Evaluator(..),
        evaluator,
        -- ** Standard evaluators
        ghciEvaluator,
        publishedActionsEvaluator,
        publishedActions,
        publishAction,
        -- * Eval\/Interpretation
        jumpToErrorE,
        jumpToE,
        consoleKeymap,
) where

import Data.Array
import Data.List
import Data.Monoid
import Data.Default
import Prelude hiding (error, (.))
import qualified Language.Haskell.Interpreter as LHI
import System.Directory(doesFileExist)
import qualified Data.HashMap.Strict as M

import Yi.Config.Simple.Types
import Yi.Core  hiding (concatMap)
import Yi.File
import Yi.Hooks
import Yi.Regex
import qualified Yi.Paths(getEvaluatorContextFilename)

-- | Runs the action, as written by the user.
--
-- The behaviour of this function can be customised by modifying the 'Evaluator' variable.
execEditorAction :: String -> YiM ()
execEditorAction = runHook execEditorActionImpl

-- | Lists the action names in scope, for use by 'execEditorAction'.
--
-- The behaviour of this function can be customised by modifying the 'Evaluator' variable.
getAllNamesInScope :: YiM [String]
getAllNamesInScope = runHook getAllNamesInScopeImpl

{- | Config variable for customising the behaviour of 'execEditorAction' and 'getAllNamesInScope'.

Set this variable using 'evaluator'. See 'ghciEvaluator' and 'finiteListEvaluator' for two implementation.
-}
data Evaluator = Evaluator {
  execEditorActionImpl :: String -> YiM (), -- ^ implementation of 'execEditorAction'
  getAllNamesInScopeImpl :: YiM [String] -- ^ implementation of 'getAllNamesInScope'
 }
 deriving(Typeable)

-- | The evaluator to use for 'execEditorAction' and 'getAllNamesInScope'.
evaluator :: Field Evaluator
evaluator = customVariable

instance Default Evaluator where def = ghciEvaluator
instance YiConfigVariable Evaluator

------------------------- Evaluator based on GHCi
newtype NamesCache = NamesCache [String] deriving (Typeable, Binary)
instance Default NamesCache where
    def = NamesCache []
instance YiVariable NamesCache

{- | Evaluator implemented by calling GHCi. This evaluator can run arbitrary expressions in the class 'YiAction'.

The following two imports are always present:

> import Yi
> import qualified Yi.Keymap as Yi.Keymap

Also, if the file

> $HOME/.config/yi/local/Env.hs

exists, it is imported unqualified.
-}
ghciEvaluator :: Evaluator
ghciEvaluator = Evaluator{..} where
    execEditorActionImpl :: String -> YiM ()
    execEditorActionImpl s = do
       contextFile <- Yi.Paths.getEvaluatorContextFilename
       haveUserContext <- io $ doesFileExist contextFile
       res <- io $ LHI.runInterpreter $ do
           LHI.set [LHI.searchPath LHI.:= []]
           LHI.set [LHI.languageExtensions LHI.:= [LHI.OverloadedStrings,
                                                   LHI.NoImplicitPrelude -- use Yi prelude instead.
                                                  ]]
           when haveUserContext $ do
              LHI.loadModules [contextFile]
              LHI.setTopLevelModules ["Env"]

           LHI.setImportsQ [("Yi", Nothing), ("Yi.Keymap",Just "Yi.Keymap")] -- Yi.Keymap: Action lives there
           LHI.interpret ("Yi.makeAction ("++s++")") (error "as" :: Action)
       case res of
           Left err -> errorEditor (show err)
           Right action -> runAction action

    getAllNamesInScopeImpl :: YiM [String]
    getAllNamesInScopeImpl = do
       NamesCache cache <- withEditor $ use dynA
       result <-if null cache then do
            res <-io $ LHI.runInterpreter $ do
                LHI.set [LHI.searchPath LHI.:= []]
                LHI.getModuleExports "Yi"
            return $ case res of
               Left err ->[show err]
               Right exports -> flattenExports exports
          else return $ sort cache
       withEditor $ assign dynA (NamesCache result)
       return result


    flattenExports :: [LHI.ModuleElem] -> [String]
    flattenExports = concatMap flattenExport

    flattenExport :: LHI.ModuleElem -> [String]
    flattenExport (LHI.Fun x) = [x]
    flattenExport (LHI.Class _ xs) = xs
    flattenExport (LHI.Data _ xs) = xs

------------------- PublishedActions evaluator
newtype PublishedActions = PublishedActions {
    publishedActions_ :: M.HashMap String Action
  } deriving(Typeable, Monoid)
instance Default PublishedActions where def = mempty
makeLensesWithSuffix "A" ''PublishedActions
instance YiConfigVariable PublishedActions

-- | Accessor for the published actions. Consider using 'publishAction'.
publishedActions :: Field (M.HashMap String Action)
publishedActions = customVariable . publishedActions_A

-- | Publish the given action, by the given name. This will overwrite any existing actions by the same name.
publishAction :: (YiAction a x, Show x) => String -> a -> ConfigM ()
publishAction s a = (%=) publishedActions (M.insert s (makeAction a))

{- | Evaluator based on a fixed list of published actions. Has a few differences from 'ghciEvaluator':

  * expressions can't be evaluated

  * all suggested actions are actually valued

  * (related to the above) doesn't contain junk actions from Prelude

  * doesn't require GHCi backend, so uses less memory
-}
publishedActionsEvaluator :: Evaluator
publishedActionsEvaluator = Evaluator{..} where
    getAllNamesInScopeImpl = (M.keys . (^. publishedActions)) <$> askCfg
    execEditorActionImpl s =
        ((M.lookup s . (^. publishedActions)) <$> askCfg) >>=
        maybe (return ()) runAction


------------------- Miscellaneous interpreter

jumpToE :: String -> Int -> Int -> YiM ()
jumpToE filename line column = do
  void $ editFile filename
  withBuffer $ do _ <- gotoLn line
                  moveXorEol column

errorRegex :: Regex
errorRegex = makeRegex "^(.+):([0-9]+):([0-9]+):.*$"

parseErrorMessage :: String -> Maybe (String, Int, Int)
parseErrorMessage ln = do
  (_,result,_) <- matchOnceText errorRegex ln
  let [_,filename,line,col] = take 3 $ map fst $ elems result
  return (filename, read line, read col)

parseErrorMessageB :: BufferM (String, Int, Int)
parseErrorMessageB = do
  ln <- readLnB
  let Just location = parseErrorMessage ln
  return location

jumpToErrorE :: YiM ()
jumpToErrorE = do
  (f,l,c) <- withBuffer parseErrorMessageB
  jumpToE f l c

prompt :: String
prompt = "Yi> "

takeCommand :: String -> String
takeCommand x | prompt `isPrefixOf` x = drop (length prompt) x
              | otherwise = x

consoleKeymap :: Keymap
consoleKeymap = do _ <- event (Event KEnter [])
                   write $ do x <- withBuffer readLnB
                              case parseErrorMessage x of
                                Just (f,l,c) -> jumpToE f l c
                                Nothing -> do withBuffer $ do
                                                p <- pointB
                                                botB
                                                p' <- pointB
                                                when (p /= p') $
                                                   insertN ("\n" ++ prompt ++ takeCommand x)
                                                insertN "\n"
                                                pt <- pointB
                                                insertN prompt
                                                bm <- getBookmarkB "errorInsert"
                                                setMarkPointB bm pt
                                              execEditorAction $ takeCommand x
