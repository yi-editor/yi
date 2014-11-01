{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Common
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements common 'ExCommand's for the Vim keymap.

module Yi.Keymap.Vim.Ex.Commands.Common
    ( parse
    , parseWithBang
    , parseWithBangAndCount
    , parseRange
    , OptionAction(..)
    , parseOption
    , filenameComplete
    , forAllBuffers
    , pureExCommand
    , impureExCommand
    , errorNoWrite
    , commandArgs
    , needsSaving
    ) where

import           Control.Applicative
import           Control.Lens (use)
import           Control.Monad
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid
import qualified Data.Text as T
import           System.Directory
import qualified Text.ParserCombinators.Parsec as P
import           Text.Read (readMaybe)
import           Yi.Buffer
import           Yi.Editor
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Ex.Types
import           Yi.Misc
import           Yi.Monad
import           Yi.Style (errorStyle)
import           Yi.Utils

parse :: P.GenParser Char () ExCommand -> EventString -> Maybe ExCommand
parse parser (Ev s) =
  either (const Nothing) Just (P.parse parser "" $ T.unpack s)


parseWithBangAndCount :: P.GenParser Char () a
                      -- ^ The command name parser.
                      -> (a -> Bool
                          -> (Maybe Int)
                          -> P.GenParser Char () ExCommand)
                      -- ^ A parser for the remaining command arguments.
                      -> EventString
                      -- ^ The string to parse.
                      -> Maybe ExCommand
parseWithBangAndCount nameParser argumentParser (Ev s) = do
    either (const Nothing) Just (P.parse parser "" $ T.unpack s)
  where
    parser = do
        mcount <- parseCount
        a      <- nameParser
        bang   <- parseBang
        argumentParser a bang mcount

parseWithBang :: P.GenParser Char () a
              -- ^ The command name parser.
              -> (a -> Bool -> P.GenParser Char () ExCommand)
              -- ^ A parser for the remaining command arguments.
              -> EventString
              -- ^ The string to parse.
              -> Maybe ExCommand
parseWithBang nameParser argumentParser (Ev s) = do
    either (const Nothing) Just (P.parse parser "" $ T.unpack s)
  where
    parser = do
        a    <- nameParser
        bang <- parseBang
        argumentParser a bang

parseBang :: P.GenParser Char () Bool
parseBang = P.string "!" *> return True <|> return False

parseCount :: P.GenParser Char () (Maybe Int)
parseCount = do
    readMaybe <$> P.many P.digit

parseRange :: P.GenParser Char s (Maybe (BufferM Region))
parseRange = fmap Just parseFullRange
         <|> fmap Just parsePointRange
         <|> return Nothing

parseFullRange :: P.GenParser Char s (BufferM Region)
parseFullRange = P.char '%' *> return (regionOfB Document)

parsePointRange :: P.GenParser Char s (BufferM Region)
parsePointRange = do
    p1 <- parseSinglePoint
    void $ P.char ','
    p2 <- parseSinglePoint2 p1
    return $ do
        p1' <- p1
        p2' <- p2
        return $ mkRegion (min p1' p2') (max p1' p2')

parseSinglePoint :: P.GenParser Char s (BufferM Point)
parseSinglePoint = parseSingleMark <|> parseLinePoint

-- | Some of the parse rules for the second point actually depend
-- on the first point. If parse rule succeeds this can result
-- in the first BufferM Point having to be run twice but this
-- probably isn't a big deal.
parseSinglePoint2 :: BufferM Point -> P.GenParser Char s (BufferM Point)
parseSinglePoint2 ptB = parseEndOfLine ptB <|> parseSinglePoint

-- | Parse a single mark, or a selection mark (< or >)
parseSingleMark :: P.GenParser Char s (BufferM Point)
parseSingleMark = P.char '\'' *> (parseSelMark <|> parseNormMark)

-- | Parse a normal mark (non-system)
parseNormMark :: P.GenParser Char s (BufferM Point)
parseNormMark = do
    c <- P.anyChar
    return $ mayGetMarkB [c] >>= \case
        Nothing -> fail $ "Mark " <> show c <> " not set"
        Just mark -> use (markPointA mark)

-- | Parse selection marks.
parseSelMark :: P.GenParser Char s (BufferM Point)
parseSelMark = do
    c <- P.oneOf "<>" 
    return $ if c == '<' then getSelectionMarkPointB else pointB

-- | Parses end of line, $, only valid for 2nd point.
parseEndOfLine :: BufferM Point -> P.GenParser Char s (BufferM Point)
parseEndOfLine ptB = P.char '$' *> return (ptB >>= eolPointB)

-- | Parses a numeric line or ".+k", k relative to current
parseLinePoint :: P.GenParser Char s (BufferM Point)
parseLinePoint = parseCurrentLinePoint <|> parseNormalLinePoint

-- | Parses .+-k
parseCurrentLinePoint :: P.GenParser Char s (BufferM Point)
parseCurrentLinePoint = do
    void $ P.char '.'
    relative <- P.optionMaybe $ do
        c <- P.oneOf "+-"
        (i :: Int) <- read <$> P.many1 P.digit
        return $ if c == '+' then i else -i
    case relative of
        Nothing -> return $ pointB >>= solPointB
        Just offset -> return $ do
            ln <- curLn
            savingPointB $ gotoLn (ln + offset) >> pointB

-- | Parses a line number
parseNormalLinePoint :: P.GenParser Char s (BufferM Point)
parseNormalLinePoint = do
    ln <- read <$> P.many1 P.digit
    return . savingPointB $ gotoLn ln >> pointB

data OptionAction = Set !Bool | Invert | Ask

parseOption :: String -> (OptionAction -> Action) -> EventString -> Maybe ExCommand
parseOption name action = parse $ do
    void $ P.string "set "
    nos <- P.many (P.string "no")
    invs <- P.many (P.string "inv")
    void $ P.string name
    bangs <- P.many (P.string "!")
    qs <- P.many (P.string "?")
    return $ pureExCommand {
        cmdShow = T.concat [ "set "
                           , T.pack $ concat nos
                           , T.pack name
                           , T.pack $ concat bangs
                           , T.pack $ concat qs ]
      , cmdAction = action $
          case fmap (not . null) [qs, bangs, invs, nos] of
              [True, _, _, _] -> Ask
              [_, True, _, _] -> Invert
              [_, _, True, _] -> Invert
              [_, _, _, True] -> Set False
              _ -> Set True
      }

removePwd :: T.Text -> YiM T.Text
removePwd path = do
  pwd' <- T.pack <$> io getCurrentDirectory
  return $! if pwd' `T.snoc` '/' `T.isPrefixOf` path
            then T.drop (1 + T.length pwd') path
            else path

filenameComplete :: T.Text -> YiM [T.Text]
filenameComplete f = case f == "%" of
  True -> do
    -- current buffer is minibuffer
    -- actual file is in the second buffer in bufferStack
    gets bufferStack >>= \case
      _ :| [] -> do
        printMsg "filenameComplete: Expected to see minibuffer!"
        return []
      _ :| bufferRef : _ -> do
        currentFileName <- fmap T.pack . withGivenBuffer bufferRef $
            fmap bufInfoFileName bufInfoB

        let sanitizedFileName = if "//" `T.isPrefixOf` currentFileName
                                then '/' `T.cons` currentFileName
                                else currentFileName

        return <$> removePwd sanitizedFileName

  False -> do
    files <- matchingFileNames Nothing f
    case files of
        [] -> return []
        [x] -> return <$> removePwd x
        xs -> sequence $ fmap removePwd xs

forAllBuffers :: MonadEditor m => (BufferRef -> m ()) -> m ()
forAllBuffers f = readEditor bufferStack >>= \(b :| bs) -> f b >> mapM_ f bs

pureExCommand :: ExCommand
pureExCommand = ExCommand {
    cmdIsPure = True
  , cmdComplete = return []
  , cmdAcceptsRange = False
  , cmdAction = undefined
  , cmdShow = undefined
  }

impureExCommand :: ExCommand
impureExCommand = pureExCommand { cmdIsPure = False }


-- | Show an error on the status line.
errorEditor :: T.Text -> EditorM ()
errorEditor s = printStatus (["error: " <> s], errorStyle)


-- | Show the common error message about an unsaved file on the status line.
errorNoWrite :: EditorM ()
errorNoWrite = errorEditor "No write since last change (add ! to override)"

-- | Useful parser for any Ex command that acts kind of like a shell
commandArgs :: P.GenParser Char () [T.Text]
commandArgs = P.many commandArg

-- | Parse a single command, with a space in front
commandArg :: P.GenParser Char () T.Text
commandArg = fmap mconcat $ P.many1 P.space *> normArg

-- | Unquoted arg, allows for escaping of \, ", ', and space. Includes quoted arg
-- as a subset, because of things like aa"bbb"
normArg :: P.GenParser Char () [T.Text]
normArg = P.many1 $
        quoteArg '\"'
    <|> quoteArg '\"'
    <|> T.singleton <$> escapeChar
    <|> T.singleton <$> P.noneOf " \"\'\\"

-- | Quoted arg with char delim. Allows same escapes, but doesn't require escaping
-- of the opposite kind or space. However, it does allow escaping opposite kind like
-- normal, as well as allowing escaping of space (is this normal behavior?).
quoteArg :: Char -> P.GenParser Char () T.Text
quoteArg delim = fmap T.pack $ P.char delim 
    *> (P.many1 $ P.noneOf (delim:"\\") <|> escapeChar)
    <* P.char delim

-- | Parser for a single escape character
escapeChar :: P.GenParser Char () Char
escapeChar = P.char '\\' *> P.oneOf " \"\'\\"

needsSaving :: BufferRef -> YiM Bool
needsSaving = findBuffer >=> maybe (return False) deservesSave
