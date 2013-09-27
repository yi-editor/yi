module Yi.Keymap.Vim2.Ex.Commands.Common
    ( parse
    , parseRange
    , OptionAction(..)
    , parseOption
    , filenameComplete
    , forAllBuffers
    , pureExCommand
    , impureExCommand
    ) where

import Prelude ()
import Yi.Prelude

import Data.Either (either)
import Data.List (isPrefixOf, drop, length)
import System.Directory
import System.FilePath
import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.Misc

parse :: P.GenParser Char () ExCommand -> String -> Maybe ExCommand
parse parser s = either (const Nothing) Just (P.parse parser "" s)

parseRange :: P.GenParser Char () LineRange
parseRange = return CurrentLineRange

data OptionAction = Set !Bool | Invert | Ask

parseOption :: String -> (OptionAction -> Action) -> String -> Maybe ExCommand
parseOption name action = parse $ do
    discard $ P.string "set "
    nos <- P.many (P.string "no")
    invs <- P.many (P.string "inv")
    discard $ P.string name
    bangs <- P.many (P.string "!")
    qs <- P.many (P.string "?")
    let 
    return $ pureExCommand {
        cmdShow = "set " ++ concat nos ++ name ++ concat bangs ++ concat qs
      , cmdAction = action $
          case (fmap (not . null) [qs, bangs, invs, nos]) of
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
