{-# LANGUAGE PatternSignatures #-}

module Yi.Eval (
        -- * Eval\/Interpretation
        jumpToErrorE,
        consoleKeymap,
) where

import Control.Monad
import Control.Monad.Trans
import Data.Array
import Data.List
import Prelude hiding (error)
import System.Directory
import Text.Regex.Posix
import Yi.Core
import Yi.Debug
import Yi.Editor
import Yi.Keymap
import Yi.Interact hiding (write)
import Yi.Event
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Dired

jumpToE :: String -> Int -> Int -> YiM ()
jumpToE filename line column = do
  bs <- readEditor $ findBufferWithName filename -- FIXME: should find by associated file-name
  case bs of
    [] -> do found <- lift $ doesFileExist filename
             if found
               then fnewE filename
               else error "file not found"
    (b:_) -> withEditor $ switchToBufferOtherWindowE b
  withBuffer $ do gotoLn line
                  moveXorEol column


parseErrorMessage :: String -> Maybe (String, Int, Int)
parseErrorMessage ln = do
  result :: (Array Int String) <- ln =~~ "^(.+):([0-9]+):([0-9]+):.*$"
  return (result!1, read (result!2), read (result!3))

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
