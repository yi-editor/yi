{-# LANGUAGE ScopedTypeVariables #-}

module Yi.Eval (
        -- * Eval\/Interpretation
        jumpToErrorE,
        jumpToE,
        consoleKeymap,
        execEditorAction
) where

import Data.Array
import Data.List
import Prelude hiding (error)
import Yi.Regex
import Yi.Config
import Yi.Core  hiding (toDyn)
import Yi.Interact hiding (write)
import Yi.Event
import Yi.Dired
import Yi.Interpreter
import Data.Dynamic
import Control.Monad.Reader (asks)
import Yi.MiniBuffer () -- instances

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

                 toDyn (makeAction :: (String -> YiM ()) -> Action),

                 toDyn (makeAction :: (String -> String -> BufferM ()) -> Action),
                 toDyn (makeAction :: (Char -> BufferM ()) -> Action),

                 toDyn (makeAction :: (BufferRef -> EditorM ()) -> Action)

                ]
            
