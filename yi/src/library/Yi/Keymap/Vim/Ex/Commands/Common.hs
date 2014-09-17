{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid
import qualified Data.Text as T
import           System.Directory
import qualified Text.ParserCombinators.Parsec as P
import           Text.Read (readMaybe)
import           Yi.Buffer
import           Yi.Editor
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

parseRange :: P.GenParser Char () LineRange
parseRange = return CurrentLineRange

parseCount :: P.GenParser Char () (Maybe Int)
parseCount = do
    readMaybe <$> P.many P.digit

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
  pwd <- T.pack <$> io getCurrentDirectory
  return $! if pwd `T.snoc` '/' `T.isPrefixOf` path
            then T.drop (1 + T.length pwd) path
            else path

filenameComplete :: T.Text -> YiM [T.Text]
filenameComplete f = case f == "%" of
  True -> do
    -- current buffer is minibuffer
    -- actual file is in the second buffer in bufferStack
    gets bufferStack >>= \case
      _ :| [] -> do
        withEditor $ printMsg "filenameComplete: Expected to see minibuffer!"
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
