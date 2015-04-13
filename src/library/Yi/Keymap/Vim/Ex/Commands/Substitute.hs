{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Substitute
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Substitute (parse) where

import           Control.Applicative              (Alternative ((<|>)), (<$>))
import           Control.Monad                    (void)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (cons, snoc)
import qualified Text.ParserCombinators.Parsec    as P (char, many, noneOf, oneOf, string, try)
import           Yi.Buffer.Adjusted
import           Yi.Editor                        (EditorM, closeBufferAndWindowE, printMsg, withCurrentBuffer)
import           Yi.Keymap                        (Action (EditorA), Keymap)
import           Yi.Keymap.Keys                   (char, choice, (?>>!))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.MiniBuffer                    (spawnMinibufferE)
import           Yi.Regex                         (makeSearchOptsM)
import qualified Yi.Rope                          as R (YiString, fromString, length, null, toText, toString)
import           Yi.Search

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    percents <- P.many (P.char '%')
    void $ P.try (P.string "substitute") <|> P.string "s"
    delimiter <- P.oneOf "!@#$%^&*()[]{}<>/.,~';:?-="
    from <- R.fromString <$> P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    to <- R.fromString <$> P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    flagChars <- P.many (P.oneOf "gic")
    return $! substitute from to delimiter
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        ('c' `elem` flagChars)
        (not $ null percents)

substitute :: R.YiString -> R.YiString -> Char -> Bool -> Bool -> Bool -> Bool -> ExCommand
substitute from to delimiter global caseInsensitive confirm allLines = Common.pureExCommand {
    cmdShow = (if allLines then "%" else "")
              <>       "substitute"
              <>       (delimiter `T.cons` R.toText from)
              <>       (delimiter `T.cons` R.toText to)
              `T.snoc` delimiter
              <>       (if confirm then "c" else "")
              <>       (if caseInsensitive then "i" else "")
              <>       (if global then "g" else "")
  , cmdAction = EditorA $ do
        let opts = QuoteRegex : if caseInsensitive then [IgnoreCase] else []
        regex <- if R.null from
                    then getRegexE
                    else return . (either (const Nothing) Just) 
                            . makeSearchOptsM opts . R.toString $ from
        case regex of
            Nothing -> printMsg "No previous search pattern"
            Just regex' -> if confirm
                then substituteConfirm regex' to global allLines
                else withCurrentBuffer $ do
                    let replace = void $ regionOfB Line
                                  >>= searchAndRepRegion0 regex' to global
                    if allLines
                        then withEveryLineB replace
                        else replace
                    moveToSol
  }

-- | Run substitution in confirm mode
substituteConfirm :: SearchExp -> R.YiString -> Bool -> Bool -> EditorM ()
substituteConfirm regex to global allLines = do
    setRegexE regex
    regions <- withCurrentBuffer $ findMatches regex global allLines
    substituteMatch to 0 False regions

-- | All matches to replace under given flags
findMatches :: SearchExp -> Bool -> Bool -> BufferM [Region]
findMatches regex global allLines = do
    lns <- if allLines
        then do lineCount <- lineCountB
                lineRegions [1..lineCount]
        else return <$> regionOfB Line
    let f = if global then id else take 1
    concat <$> mapM (fmap f . regexRegionB regex) lns

-- | Get regions corresponding to all lines
lineRegions :: [Int] -> BufferM [Region]
lineRegions = mapM $ \ln -> gotoLn ln >> regionOfB Line

-- | Offsets a region (to account for a region prior being modified)
offsetRegion :: Int -> Region -> Region
offsetRegion k reg = mkRegion (regionStart reg + k') (regionEnd reg + k')
    where k' = fromIntegral k

-- | Runs a list of matches using itself as a continuation
substituteMatch :: R.YiString -> Int -> Bool -> [Region] -> EditorM ()
substituteMatch _ _ _ [] = resetRegexE
substituteMatch to co autoAll (m:ms) = do
    let m' = offsetRegion co m
    withCurrentBuffer . moveTo $ regionStart m'
    len <- withCurrentBuffer $ R.length <$> readRegionB m'
    let diff = R.length to - len
        tex = "replace with " <> R.toText to <> " (y/n/a/q)?"
    if autoAll
        then do withCurrentBuffer $ replaceRegionB m' to
                substituteMatch to (co + diff) True ms
        else void . spawnMinibufferE tex . const $ askKeymap to co (co + diff) m ms

-- | Actual choices during confirm mode.
askKeymap :: R.YiString -> Int -> Int -> Region -> [Region] -> Keymap
askKeymap to co co' m ms = choice [
      char 'n' ?>>! cleanUp >> substituteMatch to co False ms
    , char 'a' ?>>! do cleanUp
                       replace
                       substituteMatch to co' True ms
    , char 'y' ?>>! do cleanUp
                       replace
                       substituteMatch to co' False ms
    , char 'q' ?>>! cleanUp >> resetRegexE
    ]
    where cleanUp = closeBufferAndWindowE
          replace = withCurrentBuffer $ replaceRegionB (offsetRegion co m) to
