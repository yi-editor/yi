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

import Control.Applicative
import Control.Monad
import Data.List (isPrefixOf)
import System.Directory
import qualified Text.ParserCombinators.Parsec as P
import Text.Read (readMaybe)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import Yi.Misc
import Yi.Utils
import Yi.Monad
import Yi.Style (errorStyle)

parse :: P.GenParser Char () ExCommand -> String -> Maybe ExCommand
parse parser s = either (const Nothing) Just (P.parse parser "" s)


parseWithBangAndCount :: P.GenParser Char () a
                      -- ^ The command name parser.
                      -> (a -> Bool -> (Maybe Int) -> P.GenParser Char () ExCommand)
                      -- ^ A parser for the remaining command arguments.
                      -> String
                      -- ^ The string to parse.
                      -> Maybe ExCommand
parseWithBangAndCount nameParser argumentParser s = do
    either (const Nothing) Just (P.parse parser "" s)
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
              -> String
              -- ^ The string to parse.
              -> Maybe ExCommand
parseWithBang nameParser argumentParser s = do
    either (const Nothing) Just (P.parse parser "" s)
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

parseOption :: String -> (OptionAction -> Action) -> String -> Maybe ExCommand
parseOption name action = parse $ do
    void $ P.string "set "
    nos <- P.many (P.string "no")
    invs <- P.many (P.string "inv")
    void $ P.string name
    bangs <- P.many (P.string "!")
    qs <- P.many (P.string "?")
    return $ pureExCommand {
        cmdShow = "set " ++ concat nos ++ name ++ concat bangs ++ concat qs
      , cmdAction = action $
          case fmap (not . null) [qs, bangs, invs, nos] of
              [True, _, _, _] -> Ask
              [_, True, _, _] -> Invert
              [_, _, True, _] -> Invert
              [_, _, _, True] -> Set False
              _ -> Set True
      }

removePwd :: FilePath -> YiM FilePath
removePwd path = do
    pwd <- io getCurrentDirectory
    return $! if (pwd ++ "/") `isPrefixOf` path
              then drop (1 + length pwd) path
              else path

filenameComplete :: FilePath -> YiM [FilePath]
filenameComplete "%" = do
    -- current buffer is minibuffer
    -- actual file is in the second buffer in bufferStack
    bufferRef <- withEditor $ gets (head . drop 1 . bufferStack)
    currentFileName <- withGivenBuffer bufferRef $
        fmap bufInfoFileName bufInfoB

    let sanitizedFileName = case currentFileName of
                            ('/':'/':f') -> '/':f'
                            _ -> currentFileName

    fmap (:[]) $ removePwd sanitizedFileName

filenameComplete f = do
    files <- matchingFileNames Nothing f

    case files of
        [] -> return []
        [x] -> fmap (:[]) $ removePwd x
        xs -> sequence $ fmap removePwd xs

forAllBuffers :: MonadEditor m => (BufferRef -> m ()) -> m ()
forAllBuffers f = mapM_ f =<< readEditor bufferStack

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
errorEditor :: String -> EditorM ()
errorEditor s = printStatus (["error: " ++ s], errorStyle)


-- | Show the common error message about an unsaved file on the status line.
errorNoWrite :: EditorM ()
errorNoWrite = errorEditor "No write since last change (add ! to override)"
