{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
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
    , BoolOptionAction(..)
    , TextOptionAction(..)
    , parseBoolOption
    , parseTextOption
    , filenameComplete
    , forAllBuffers
    , pureExCommand
    , impureExCommand
    , errorNoWrite
    , commandArgs
    , needsSaving
    ) where

import           Control.Applicative           (Alternative ((<|>)))
import           Lens.Micro.Platform                    (use)
import           Control.Monad                 (void, (>=>))
import qualified Data.Attoparsec.Text          as P (Parser, anyChar, char,
                                                     digit, inClass, many',
                                                     many1, notInClass, parseOnly,
                                                     option, satisfy, space, string)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T (Text, concat, cons, drop,
                                                     isPrefixOf, length, pack,
                                                     singleton, snoc)
import           System.Directory              (getCurrentDirectory)
import           Text.Read                     (readMaybe)
import           Yi.Buffer
import           Yi.Editor
import           Yi.File                       (deservesSave)
import           Yi.Keymap                     (Action, YiM, readEditor)
import           Yi.Keymap.Vim.Common          (EventString (Ev))
import           Yi.Keymap.Vim.Ex.Types        (ExCommand (..))
import           Yi.Misc                       (matchingFileNames)
import           Yi.Monad                      (gets)
import           Yi.Style                      (errorStyle)
import           Yi.Utils                      (io)

-- TODO this kind of thing is exactly where it makes sense to
-- *not* use parseOnly but its easier to have compatibility with
-- the old parsec-based interface for now.

parse :: P.Parser ExCommand -> EventString -> Maybe ExCommand
parse parser (Ev s) =
  either (const Nothing) Just $ P.parseOnly parser s

parseWithBangAndCount :: P.Parser a
                      -- ^ The command name parser.
                      -> (a -> Bool
                          -> Maybe Int
                          -> P.Parser ExCommand)
                      -- ^ A parser for the remaining command arguments.
                      -> EventString
                      -- ^ The string to parse.
                      -> Maybe ExCommand
parseWithBangAndCount nameParser argumentParser (Ev s) =
    either (const Nothing) Just (P.parseOnly parser s)
  where
    parser = do
        mcount <- parseCount
        a      <- nameParser
        bang   <- parseBang
        argumentParser a bang mcount

parseWithBang :: P.Parser a
              -- ^ The command name parser.
              -> (a -> Bool -> P.Parser ExCommand)
              -- ^ A parser for the remaining command arguments.
              -> EventString
              -- ^ The string to parse.
              -> Maybe ExCommand
parseWithBang nameParser argumentParser (Ev s) =
    either (const Nothing) Just (P.parseOnly parser s)
  where
    parser = do
        a    <- nameParser
        bang <- parseBang
        argumentParser a bang

parseBang :: P.Parser Bool
parseBang = P.string "!" *> return True <|> return False

parseCount :: P.Parser (Maybe Int)
parseCount = readMaybe <$> P.many' P.digit

parseRange :: P.Parser (Maybe (BufferM Region))
parseRange = fmap Just parseFullRange
         <|> fmap Just (styleRange parsePointRange)
         <|> return Nothing

styleRange :: P.Parser (BufferM Region) -> P.Parser (BufferM Region)
styleRange = fmap $ \regionB -> do
    region <- regionB
    convertRegionToStyleB region LineWise

parseFullRange :: P.Parser (BufferM Region)
parseFullRange = P.char '%' *> return (regionOfB Document)

parsePointRange :: P.Parser (BufferM Region)
parsePointRange = do
    p1 <- parseSinglePoint
    void $ P.char ','
    p2 <- parseSinglePoint2 p1
    return $ do
        p1' <- p1
        p2' <- p2
        return $ mkRegion (min p1' p2') (max p1' p2')

parseSinglePoint :: P.Parser (BufferM Point)
parseSinglePoint = parseSingleMark <|> parseLinePoint

-- | Some of the parse rules for the second point actually depend
-- on the first point. If parse rule succeeds this can result
-- in the first BufferM Point having to be run twice but this
-- probably isn't a big deal.
parseSinglePoint2 :: BufferM Point -> P.Parser (BufferM Point)
parseSinglePoint2 ptB = parseEndOfLine ptB <|> parseSinglePoint

-- | Parse a single mark, or a selection mark (< or >)
parseSingleMark :: P.Parser (BufferM Point)
parseSingleMark = P.char '\'' *> (parseSelMark <|> parseNormMark)

-- | Parse a normal mark (non-system)
parseNormMark :: P.Parser (BufferM Point)
parseNormMark = do
    c <- P.anyChar
    return $ mayGetMarkB [c] >>= \case
        Nothing -> fail $ "Mark " <> show c <> " not set"
        Just mark -> use (markPointA mark)

-- | Parse selection marks.
parseSelMark :: P.Parser (BufferM Point)
parseSelMark = do
    c <- P.satisfy $ P.inClass "<>"
    return $ if c == '<' then getSelectionMarkPointB else pointB

-- | Parses end of line, $, only valid for 2nd point.
parseEndOfLine :: BufferM Point -> P.Parser (BufferM Point)
parseEndOfLine ptB = P.char '$' *> return (ptB >>= eolPointB)

-- | Parses a numeric line or ".+k", k relative to current
parseLinePoint :: P.Parser (BufferM Point)
parseLinePoint = parseCurrentLinePoint <|> parseNormalLinePoint

-- | Parses .+-k
parseCurrentLinePoint :: P.Parser (BufferM Point)
parseCurrentLinePoint = do
    relative <- (Nothing <$ P.char '.' <|>) $
      do () <$ P.char '.' <|> pure ()
         c <- P.satisfy $ P.inClass "+-"
         (i :: Int) <- read <$> P.many1 P.digit
         return . Just $ if c == '+' then i else -i
    case relative of
        Nothing -> return $ pointB >>= solPointB
        Just offset -> return $ do
            ln <- curLn
            savingPointB $ gotoLn (ln + offset) >> pointB

-- | Parses a line number
parseNormalLinePoint :: P.Parser (BufferM Point)
parseNormalLinePoint = do
    ln <- read <$> P.many1 P.digit
    return . savingPointB $ gotoLn ln >> pointB

data BoolOptionAction = BoolOptionSet !Bool | BoolOptionInvert | BoolOptionAsk

parseBoolOption :: T.Text -> (BoolOptionAction -> Action) -> EventString
    -> Maybe ExCommand
parseBoolOption name action = parse $ do
    void $ P.string "set "
    nos <- P.many' (P.string "no")
    invs <- P.many' (P.string "inv")
    void $ P.string name
    bangs <- P.many' (P.string "!")
    qs <- P.many' (P.string "?")
    return $ pureExCommand {
        cmdShow = T.concat [ "set "
                           , T.concat nos
                           , name
                           , T.concat bangs
                           , T.concat qs ]
      , cmdAction = action $
          case fmap (not . null) [qs, bangs, invs, nos] of
              [True, _, _, _] -> BoolOptionAsk
              [_, True, _, _] -> BoolOptionInvert
              [_, _, True, _] -> BoolOptionInvert
              [_, _, _, True] -> BoolOptionSet False
              _ -> BoolOptionSet True
      }

data TextOptionAction = TextOptionSet !T.Text | TextOptionAsk

parseTextOption :: T.Text -> (TextOptionAction -> Action) -> EventString
    -> Maybe ExCommand
parseTextOption name action = parse $ do
    void $ P.string "set "
    void $ P.string name
    maybeNewValue <- P.option Nothing $ Just <$> do
        void $ P.many' P.space
        void $ P.char '='
        void $ P.many' P.space
        T.pack <$> P.many' P.anyChar
    return $ pureExCommand
      { cmdShow = T.concat [ "set "
                           , name
                           , maybe "" (" = " <>) maybeNewValue
                           ]
      , cmdAction = action $ maybe TextOptionAsk TextOptionSet maybeNewValue
      }

removePwd :: T.Text -> YiM T.Text
removePwd path = do
  pwd' <- T.pack <$> io getCurrentDirectory
  return $! if pwd' `T.snoc` '/' `T.isPrefixOf` path
            then T.drop (1 + T.length pwd') path
            else path

filenameComplete :: T.Text -> YiM [T.Text]
filenameComplete f = if f == "%"
  then
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

  else do
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
commandArgs :: P.Parser [T.Text]
commandArgs = P.many' commandArg

-- | Parse a single command, with a space in front
commandArg :: P.Parser T.Text
commandArg = fmap mconcat $ P.many1 P.space *> normArg

-- | Unquoted arg, allows for escaping of \, ", ', and space. Includes quoted arg
-- as a subset, because of things like aa"bbb"
normArg :: P.Parser [T.Text]
normArg = P.many1 $
        quoteArg '\"'
    <|> quoteArg '\"'
    <|> T.singleton <$> escapeChar
    <|> T.singleton <$> P.satisfy (P.notInClass " \"\'\\")

-- | Quoted arg with char delim. Allows same escapes, but doesn't require escaping
-- of the opposite kind or space. However, it does allow escaping opposite kind like
-- normal, as well as allowing escaping of space (is this normal behavior?).
quoteArg :: Char -> P.Parser T.Text
quoteArg delim = fmap T.pack $ P.char delim 
    *> P.many1 (P.satisfy (P.notInClass (delim:"\\")) <|> escapeChar)
    <* P.char delim

-- | Parser for a single escape character
escapeChar :: P.Parser Char
escapeChar = P.char '\\' *> P.satisfy (P.inClass " \"\'\\")

needsSaving :: BufferRef -> YiM Bool
needsSaving = findBuffer >=> maybe (return False) deservesSave
