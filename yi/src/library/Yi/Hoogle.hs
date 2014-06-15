-- | Provides functions for calling Hoogle on the commandline, and processing results
-- into a form useful for completion or insertion.
module Yi.Hoogle where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.Char (isUpper)
import Data.List (isInfixOf, nub)
import System.Exit (ExitCode(ExitFailure))

import Yi.Core
import Yi.Process (runProgCommand)
import Yi.Utils

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
hoogleRaw srch opts = do
  let options = filter (not . null) [opts, srch]
  outp@(_status, out, _err) <- runProgCommand "hoogle" options
  case outp of
    (ExitFailure 1, "", "") -> -- no output, probably failed to run binary
      fail "Error running hoogle command.  Is hoogle on path?"
    (ExitFailure 1, xs, _) -> fail $ "hoogle failed with: " ++ xs
    _ -> return ()
  let results = lines out
  if results == ["No results found"]
    then fail "No Hoogle results"
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

-- | Call out to 'hoogleRaw', and print inside the Minibuffer the results of
-- searching Hoogle with the word at point.
hoogleSearch :: YiM ()
hoogleSearch = do
  word <- withBuffer $ do wordRegion <- regionOfB unitWord
                          readRegionB wordRegion
  results <- io $ hoogleRaw word ""

  -- The quotes help legibility between closely-packed results
  withEditor $ printMsgs $ map show results
