-- | Provides functions for calling Hoogle on the commandline, and processing results
-- into a form useful for completion or insertion.
module Yi.Hoogle where

import Prelude ()
import Control.Arrow ((&&&))
import Data.Char (isUpper)
import Data.List (isInfixOf, nub, filter, lines, words, map, (!!))
import System.Exit (ExitCode(ExitFailure))

import Yi.Core
import Yi.Process (runProgCommand)
import Yi.Buffer (replaceRegionB, unitWord)

-- | Remove anything starting with uppercase letter. These denote either module names or types.
caseSensitize :: [String] -> [String]
caseSensitize = filter (not . isUpper . head)

-- | Hoogle's output includes a sort of type keyword, telling whether a hit is a package name, syntax,
-- a module name, etc. But we care primarily about the function names, so we filter out anything containing
-- the keywords.
gv :: [String] -> [String]
gv = filter f
  where f x = not $ any (`isInfixOf` x) ["module ", " type ", "package ", " data ", " keyword "]

-- | Query Hoogle, with given search and options. This errors out on no
-- results or if the hoogle command is not on path.
hoogleRaw :: String -> String -> IO [String]
hoogleRaw srch opts = do (out,_err,status) <- runProgCommand "hoogle" [opts, srch]
                         when (status == ExitFailure 1) $
                             fail "Error running hoogle command.  Is hoogle \
                                  \on path?"
                         let results = lines out
                         if results == ["No results found"] then fail "No Hoogle results"
                                                            else return results

-- | Filter the output of 'hoogleRaw' to leave just functions.
hoogleFunctions :: String -> IO [String]
hoogleFunctions a = caseSensitize . gv . nub . map ((!!1) . words) <$> hoogleRaw a ""

-- | Return module-function pairs.
hoogleFunModule :: String -> IO [(String, String)]
hoogleFunModule a = map ((head &&& (!! 1)) . words) . gv  <$> hoogleRaw a ""

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
