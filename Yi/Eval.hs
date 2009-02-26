{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators, DeriveDataTypeable #-}

module Yi.Eval (
        -- * Eval\/Interpretation
        jumpToErrorE,
        jumpToE,
        consoleKeymap,
        execEditorAction,
        getAllNamesInScope
) where

import Data.Array
import Data.List
import Prelude hiding (error, (.))
import Yi.Regex
import Yi.Core  hiding (toDyn, concatMap)
import Yi.Interact hiding (write)
import Yi.Event
import Yi.Dired
import Data.Dynamic
#ifdef GHC_INTERPRETER
import qualified Language.Haskell.Interpreter as LHI
import Control.Monad
#else
import qualified Data.Map as M
import Control.Monad.Reader (asks)
import Yi.Config
import Yi.Interpreter
import Yi.MiniBuffer (FilePatternTag, RegexTag, (:::))
#endif

#ifdef GHC_INTERPRETER

-- | Returns an Interpreter action that loads the desired modules and interprets the expression.
execEditorAction :: String -> YiM ()
execEditorAction s = do
   res <-io $ LHI.runInterpreter $ do
       LHI.set [LHI.searchPath LHI.:= []]
       LHI.set [LHI.languageExtensions := [LHI.OverloadedStrings]]
       LHI.setImportsQ [("Yi", Nothing), ("Yi.Keymap",Just "Yi.Keymap")] -- Yi.Keymap: Action lives there
       LHI.interpret ("Yi.makeAction ("++s++")") (error "as" :: Action)
   case res of
       Left err ->errorEditor (show err)
       Right action -> runAction action

data NamesCache = NamesCache [String] deriving Typeable
instance Initializable NamesCache where
    initial = NamesCache []
 
getAllNamesInScope :: YiM [String]
getAllNamesInScope = do 
   NamesCache cache <-withEditor $ getA dynA
   result <-if null cache then do
        res <-io $ LHI.runInterpreter $ do
            LHI.set [LHI.searchPath LHI.:= []]
            LHI.getModuleExports "Yi"
        return $ case res of
           Left err ->[show err]
           Right exports -> flattenExports exports
      else return cache
   withEditor $ putA dynA (NamesCache result)
   return result
  

flattenExports :: [LHI.ModuleElem] -> [String]
flattenExports = concatMap flattenExport

flattenExport :: LHI.ModuleElem -> [String]
flattenExport (LHI.Fun x) = [x]
flattenExport (LHI.Class _ xs) = xs
flattenExport (LHI.Data _ xs) = xs
#else
getAllNamesInScope :: YiM [String]
getAllNamesInScope = do 
  acts <- asks (publishedActions . yiConfig)
  return (M.keys acts)


execEditorAction :: String -> YiM ()
execEditorAction s = do 
  env <- asks (publishedActions . yiConfig)
  case toMono =<< interpret =<< addMakeAction =<< rename env =<< parse s of
    Left err -> errorEditor err
    Right a -> runAction a
  where addMakeAction expr = return $ UApp (UVal mkAct) expr
        mkAct = [
                 toDyn (makeAction :: BufferM () -> Action),
                 toDyn (makeAction :: BufferM Bool -> Action),
                 toDyn (makeAction :: BufferM Char -> Action),
                 toDyn (makeAction :: BufferM Int -> Action),
                 toDyn (makeAction :: BufferM String -> Action),
                 toDyn (makeAction :: BufferM Region -> Action),
                 toDyn (makeAction :: BufferM Mark -> Action),
                 toDyn (makeAction :: BufferM MarkValue -> Action),

                 toDyn (makeAction :: (String -> BufferM ()) -> Action),
                 toDyn (makeAction :: (AnyMode -> BufferM ()) -> Action),

                 toDyn (makeAction :: EditorM () -> Action),

                 toDyn (makeAction :: YiM () -> Action),
                 toDyn (makeAction :: YiM BufferRef -> Action),

                 toDyn (makeAction :: (String ::: RegexTag -> YiM ()) -> Action),
                 toDyn (makeAction :: (String ::: FilePatternTag -> String ::: RegexTag -> YiM ()) -> Action),
                 toDyn (makeAction :: (String -> YiM ()) -> Action),

                 toDyn (makeAction :: (String -> String -> BufferM ()) -> Action),
                 toDyn (makeAction :: (Char -> BufferM ()) -> Action),

                 toDyn (makeAction :: (BufferRef -> EditorM ()) -> Action)
                ]
#endif

jumpToE :: String -> Int -> Int -> YiM ()
jumpToE filename line column = do
  fnewE filename
  withBuffer $ do gotoLn line
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
consoleKeymap = do event (Event KEnter [])
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
