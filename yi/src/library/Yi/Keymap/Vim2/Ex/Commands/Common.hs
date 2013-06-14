module Yi.Keymap.Vim2.Ex.Commands.Common
    ( parse
    , parseRange
    , filenameComplete
    , forAllBuffers
    ) where

import Prelude ()
import Yi.Prelude

import Data.Either (either)
import Data.List
import System.Directory
import System.FilePath
import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.Misc

parse :: P.GenParser Char () a -> String -> Maybe a
parse parser s = either (const Nothing) Just (P.parse parser "" s)

parseRange :: P.GenParser Char () LineRange
parseRange = return CurrentLineRange

removePwd :: FilePath -> YiM FilePath
removePwd path = do
    pwd <- io getCurrentDirectory
    return $! if (pwd ++ "/") `isPrefixOf` path
              then drop (1 + length pwd) path
              else path

filenameComplete :: FilePath -> YiM (Maybe FilePath)
filenameComplete "%" = do
    -- current buffer is minibuffer
    -- actual file is in the second buffer in bufferStack
    bufferRef <- withEditor $ gets (head . drop 1 . bufferStack)
    currentFileName <- withGivenBuffer bufferRef $
        fmap bufInfoFileName bufInfoB

    let sanitizedFileName = case currentFileName of
                            ('/':'/':f') -> '/':f'
                            _ -> currentFileName

    fmap Just $ removePwd sanitizedFileName

filenameComplete f = do
    files <- matchingFileNames Nothing f

    case files of
        [] -> return Nothing
        [x] -> fmap Just $ removePwd x 
        xs -> fmap Just $ removePwd (commonPrefix xs)

forAllBuffers :: MonadEditor m => (BufferRef -> m ()) -> m ()
forAllBuffers f = mapM_ f =<< readEditor bufferStack