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

import           Control.Applicative              (Alternative)
import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (char, inClass, many', match,
                                                        satisfy, string, option,
                                                        (<?>), Parser)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (Text, cons, snoc)
import           Lens.Micro.Platform              (over, _2)
import           Yi.Buffer.Adjusted
import           Yi.Editor                        (EditorM, closeBufferAndWindowE, printMsg, withCurrentBuffer)
import           Yi.Keymap                        (Action (EditorA), Keymap)
import           Yi.Keymap.Keys                   (char, choice, (?>>!))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand, parseRange)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.MiniBuffer                    (spawnMinibufferE)
import           Yi.Regex                         (makeSearchOptsM)
import qualified Yi.Rope                          as R (YiString, fromString, length, null, toText, toString)
import           Yi.Search

-- | Skip one or no occurrences of a given parser.
skipOptional :: Alternative f => f a -> f ()
skipOptional p = P.option () (() <$ p)
{-# SPECIALIZE skipOptional :: P.Parser a -> P.Parser () #-}

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    (rangeText, rangeB) <- over _2 (fromMaybe $ regionOfB Line) <$> P.match Common.parseRange
    P.char 's' *> 
      skipOptional (P.string "ub" *> skipOptional (P.string "stitute"))
      P.<?> "substitute"
    delimiter <- P.satisfy (`elem` ("!@#$%^&*()[]{}<>/.,~';:?-=" :: String))
    from <- R.fromString <$> P.many' (P.satisfy (/= delimiter))
    void $ P.char delimiter
    to <- R.fromString <$> P.many' (P.satisfy (/= delimiter))
    flagChars <- P.option "" $
      P.char delimiter *> P.many' (P.satisfy $ P.inClass "gic")
    return $! substitute from to delimiter
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        ('c' `elem` flagChars)
        rangeText
        rangeB

substitute :: R.YiString -> R.YiString -> Char -> Bool -> Bool -> Bool -> T.Text -> BufferM Region -> ExCommand
substitute from to delimiter global caseInsensitive confirm regionText regionB = Common.pureExCommand {
    cmdShow = regionText
              <>       "s"
              <>       (delimiter `T.cons` R.toText from)
              <>       (delimiter `T.cons` R.toText to)
              `T.snoc` delimiter
              <>       (if confirm then "c" else "")
              <>       (if caseInsensitive then "i" else "")
              <>       (if global then "g" else "")
  , cmdAction = EditorA $ do
        let opts = if caseInsensitive then [IgnoreCase] else []
        lines' <- withCurrentBuffer $ regionB >>= linesOfRegionB
        regex <- if R.null from
                    then getRegexE
                    else return . (either (const Nothing) Just) 
                            . makeSearchOptsM opts . R.toString $ from
        case regex of
            Nothing -> printMsg "No previous search pattern"
            Just regex' -> if confirm
                then substituteConfirm regex' to global lines'
                else withCurrentBuffer $ do
                    -- We need to reverse the lines' here so that replacing
                    -- does not effect the regions in question.
                    mapM_ (void . searchAndRepRegion0 regex' to global) (reverse lines')
                    moveToSol
  }

-- | Run substitution in confirm mode
substituteConfirm :: SearchExp -> R.YiString -> Bool -> [Region] -> EditorM ()
substituteConfirm regex to global lines' = do
    -- TODO This highlights all matches, even in non-global mode
    -- and could potentially be classified as a bug. Fixing requires
    -- changing the regex highlighting api.
    setRegexE regex
    regions <- withCurrentBuffer $ findMatches regex global lines'
    substituteMatch to 0 False regions

-- | All matches to replace under given flags
findMatches :: SearchExp -> Bool -> [Region] -> BufferM [Region]
findMatches regex global lines' = do
    let f = if global then id else take 1
    concat <$> mapM (fmap f . regexRegionB regex) lines'

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
