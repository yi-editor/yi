module Yi.Eval (
        -- * Eval\/Interpretation
        evalE,
        execE,
) where

import Prelude hiding (error)

import Yi.Debug
import Yi.Editor
import Yi.Core
import Yi.Kernel

import Data.Maybe
import Data.Dynamic
import Data.List

import Control.Monad.Reader
import Control.Exception

import qualified GHC
import GHC.Exts ( unsafeCoerce# )


evalToStringE :: String -> EditorM String
evalToStringE string = withKernel $ \kernel -> do
  yiContext kernel
  result <- compileExpr kernel ("show (" ++ string ++ ")")
  case result of
    Nothing -> return ""
    Just x -> return (unsafeCoerce# x)

-- | Evaluate some text and show the result in the message line.
evalE :: String -> EditorM ()
evalE s = evalToStringE s >>= msgE

-- | Run a (dynamically specified) editor command.
execE :: String -> EditorM ()
execE s = ghcErrorHandlerE $ do
            result <- withKernel $ \kernel -> do
                               logPutStrLn $ "execing " ++ s
                               compileExpr kernel ("(" ++ s ++ ") >>= msgE . show :: EditorM ()")
            case result of
              Nothing -> errorE "Could not compile expression"
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

catchDynE :: Typeable exception => EditorM a -> (exception -> EditorM a) -> EditorM a
catchDynE inner handler = ReaderT (\r -> catchDyn (runReaderT inner r) (\e -> runReaderT (handler e) r))


