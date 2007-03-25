module Yi.Eval (
        -- * Eval\/Interpretation
        evalE,
        execE,
        jumpToErrorE,
) where

import Control.Monad.Reader
import Control.Monad.Trans
import Data.Array
import GHC.Exts ( unsafeCoerce# )
import Prelude hiding (error)
import System.Directory
import Text.Regex.Posix
import Yi.Core
import Yi.Debug
import Yi.Editor
import Yi.Kernel
import Yi.UI as UI
import qualified GHC


evalToStringE :: String -> EditorM String
evalToStringE string = withKernel $ \kernel -> do
  result <- compileExpr kernel ("show (" ++ string ++ ")")
  case result of
    Nothing -> return ""
    Just x -> return (unsafeCoerce# x)

-- | Evaluate some text and show the result in the message line.
evalE :: String -> EditorM ()
evalE s = evalToStringE s >>= msgE

-- | Run a (dynamically specified) editor command.
execE :: String -> EditorM ()
execE s = do
  ghcErrorHandlerE $ do
            result <- withKernel $ \kernel -> do
                               logPutStrLn $ "execing " ++ s
                               compileExpr kernel ("(" ++ s ++ ") >>= msgE . show :: EditorM ()")
            case result of
              Nothing -> errorE ("Could not compile: " ++ s)
              Just x -> do let (x' :: EditorM ()) = unsafeCoerce# x
                           x'
                           return ()

-- | Install some default exception handlers and run the inner computation.
ghcErrorHandlerE :: EditorM () -> EditorM ()
ghcErrorHandlerE inner = do
  flip catchDynE (\dyn -> do
  		    case dyn of
		     GHC.PhaseFailed _ code -> errorE $ "Exitted with " ++ show code
		     GHC.Interrupted -> errorE $ "Interrupted!"
		     _ -> do errorE $ "GHC exeption: " ++ (show (dyn :: GHC.GhcException))
			     
	    ) $
            inner

jumpToE :: String -> Int -> Int -> EditorM ()
jumpToE filename line column = do
  bs <- readEditor $ \e -> findBufferWithName e filename
  case bs of
    [] -> do found <- lift $ doesFileExist filename
             if found 
               then fnewE filename
               else error "file not found"
    (b:_) -> getWindow >>= UI.setWindowBuffer b
  gotoLnE line
  rightOrEolE column

parseErrorMessageE :: EditorM (String, Int, Int)
parseErrorMessageE = do
  ln <- readLnE 
  result :: Array Int String <- ln =~~ "^(.+):([0-9]+):([0-9]+):.*$"
  return (result!1, read (result!2), read (result!3))

jumpToErrorE :: EditorM ()
jumpToErrorE = do
  (f,l,c) <- parseErrorMessageE
  jumpToE f l c
