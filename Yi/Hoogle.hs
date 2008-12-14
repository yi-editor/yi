-- | Provides functions for calling Hoogle on the commandline, and processing results
-- into a form useful for completion or insertion.
module Yi.Hoogle where

import Control.Monad (liftM)
import Data.Char (isUpper)
import Data.List (isInfixOf, nub)
import System.IO.Unsafe (unsafePerformIO)

import Yi hiding ((.), error)
import Yi.Process (runShellCommand)
import Yi.Core (msgEditor)
import Yi.Buffer (replaceRegionB, unitWord)

-- | Remove anything starting with uppercase letter. These denote either module names or types.
caseSensitize :: [String] -> [String]
caseSensitize [] = []
caseSensitize a = filter (\x -> not $ isUpper $ head x) a

-- | Hoogle's output includes a sort of type keyword, telling whether a hit is a package name, syntax,
-- a module name, etc. But we care primarily about the function names, so we filter out anything containing
-- the keywords.
gv :: [String] -> [String]
gv = filter (\x -> not $ "module " `isInfixOf` x || " type " `isInfixOf` x || "package " `isInfixOf` x || " data " `isInfixOf`  x || " keyword " `isInfixOf` x)

-- | Query Hoogle, with given search and options. This errors out on no results.
hoogleRaw :: String -> String -> IO [String]
hoogleRaw srch opts = do (out,_err,_status) <- runShellCommand cmd
                         let results = lines out
                         if results == ["No results found"] then error "No Hoogle results" else
                          return $ results
                         where cmd = "hoogle " ++ opts ++ " " ++ srch

-- | Filter the output of 'hoogleRaw' to leave just functions.
hoogleFunctions :: String -> IO [String]
hoogleFunctions a = do let results = hoogleRaw a ""
                       -- drop the module prefixes
                       functions <- liftM (caseSensitize . gv . nub . map (\x -> (words x) !! 1)) results
                       return functions

-- | Return module-function pairs.
hoogleFunModule :: String -> IO [(String, String)]
hoogleFunModule a = do let results = hoogleRaw a ""
                       clean <- liftM gv results                       
                       let ws = map words clean
                       let modules = map head ws
                       let functions = (map $ \x -> x !! 1) ws
                       return $ Prelude.zip modules functions

-- | Call out to 'hoogleFunModule', and overwrite the word at point with
-- the first returned function.
hoogle :: YiM String
hoogle = do 
    (wordRegion,word) <- withBuffer $ do wordRegion <- regionOfB unitWord
                                         word <- readRegionB wordRegion
                                         return (wordRegion, word)
    ((modl,fun):_) <- io $ hoogleFunModule word
    withBuffer $ replaceRegionB wordRegion fun
    return modl
