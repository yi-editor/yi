{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Hoogle
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functions for calling Hoogle on the commandline, and
-- processing results into a form useful for completion or insertion.

module Yi.Hoogle where

import           Control.Applicative ((<$>))
import           Control.Arrow       ((&&&))
import           Data.Char           (isUpper)
import           Data.List           (nub)
import qualified Data.Text           as T (isInfixOf, lines, pack)
import           System.Exit         (ExitCode (ExitFailure))
import           Yi.Buffer           (readRegionB, regionOfB, replaceRegionB, unitWord)
import           Yi.Editor           (printMsgs, withCurrentBuffer)
import           Yi.Keymap           (YiM)
import           Yi.Process          (runProgCommand)
import qualified Yi.Rope             as R (YiString, fromText, head, null, toString, toText, words)
import           Yi.String           (showT)
import           Yi.Utils            (io)

-- | Remove anything starting with uppercase letter. These denote
-- either module names or types.
caseSensitize :: [R.YiString] -> [R.YiString]
caseSensitize = filter p
  where
    p :: R.YiString -> Bool
    p t = case R.head t of
      Nothing -> False
      Just c  -> not $ isUpper c

-- | Hoogle's output includes a sort of type keyword, telling whether
-- a hit is a package name, syntax, a module name, etc. But we care
-- primarily about the function names, so we filter out anything
-- containing the keywords.
gv :: [R.YiString] -> [R.YiString]
gv = filter f
  where
    ks = ["module ", " type ", "package ", " data ", " keyword "]
    f x = not $ any (`T.isInfixOf` R.toText x) ks

-- | Query Hoogle, with given search and options. This errors out on no
-- results or if the hoogle command is not on path.
hoogleRaw :: R.YiString -> R.YiString -> IO [R.YiString]
hoogleRaw srch opts = do
  let options = filter (not . R.null) [opts, srch]
  outp@(_status, out, _err) <- runProgCommand "hoogle" (R.toString <$> options)
  case outp of
    (ExitFailure 1, "", "") -> -- no output, probably failed to run binary
      fail "Error running hoogle command.  Is hoogle on path?"
    (ExitFailure 1, xs, _) -> fail $ "hoogle failed with: " ++ xs
    _ -> return ()
  -- TODO: bench ‘R.fromText . T.lines . T.pack’ vs ‘R.lines . R.fromString’
  let results = fmap R.fromText . T.lines $ T.pack out
  if results == ["No results found"]
    then fail "No Hoogle results"
    else return results

-- | Filter the output of 'hoogleRaw' to leave just functions.
hoogleFunctions :: R.YiString -> IO [R.YiString]
hoogleFunctions a =
  caseSensitize . gv . nub . map ((!!1) . R.words) <$> hoogleRaw a ""

-- | Return module-function pairs.
hoogleFunModule :: R.YiString -> IO [(R.YiString, R.YiString)]
hoogleFunModule a = map ((head &&& (!! 1)) . R.words) . gv <$> hoogleRaw a ""

-- | Call out to 'hoogleFunModule', and overwrite the word at point with
-- the first returned function.
hoogle :: YiM R.YiString
hoogle = do
    (wordRegion,word) <- withCurrentBuffer $ do
      wordRegion <- regionOfB unitWord
      word <- readRegionB wordRegion
      return (wordRegion, word)
    ((modl,fun):_) <- io $ hoogleFunModule word

    withCurrentBuffer $ replaceRegionB wordRegion fun
    return modl

-- | Call out to 'hoogleRaw', and print inside the Minibuffer the results of
-- searching Hoogle with the word at point.
hoogleSearch :: YiM ()
hoogleSearch = do
  word <- withCurrentBuffer $ do
    wordRegion <- regionOfB unitWord
    readRegionB wordRegion
  results <- io $ hoogleRaw word ""

  -- The quotes help legibility between closely-packed results
  printMsgs $ map showT results
