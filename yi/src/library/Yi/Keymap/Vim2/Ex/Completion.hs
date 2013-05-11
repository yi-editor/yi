module Yi.Keymap.Vim2.Ex.Completion
    ( exComplete
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char
import Data.List (drop, dropWhile, length, isSuffixOf)
import System.FilePath (takeFileName)

import Yi.Buffer
import Yi.Completion
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.MiniBuffer
import Yi.Misc

exComplete :: String -> Maybe ExCommand -> YiM ()
exComplete _ (Just (ExImpure (ExEdit f))) = exFileNameComplete f
exComplete _ (Just (ExImpure (ExTabedit f))) = exFileNameComplete f
exComplete _ _ = return ()

exFileNameComplete :: String -> YiM ()
exFileNameComplete "%" = do
    -- current buffer is minibuffer
    -- actual file is in the second buffer in bufferStack
    bufferRef <- withEditor $ gets (head . drop 1 . bufferStack)
    currentFileName <- withGivenBuffer bufferRef $
        fmap bufInfoFileName bufInfoB

    let sanitizedFileName = case currentFileName of
                            ('/':'/':f') -> '/':f'
                            _ -> currentFileName

    -- now modifying minibuffer
    withBuffer $ do
        point <- pointB
        deleteNAt Forward 1 (point-1)
        insertN sanitizedFileName
exFileNameComplete filename = mkCompleteFn (completeInListCustomShow basename)
                                     prefixMatch (matchingFileNames Nothing) s >>=
                            withBuffer . insertN . drop (length s)
    where s = dropWhile isSpace filename

          -- this tries to resemble 'basename' utility:
          --   basename "foo/bar.baz" = "bar.baz"
          --   basename "foo/bar/" = "bar"
          -- but
          --   System.FilePath.takeBaseName "foo/bar.baz" = "bar"
          --   System.FilePath.takeFileName "foo/bar/" = ""
          basename f = takeFileName $ if "/" `isSuffixOf` f then init f
                                                            else f
