module Yi.File (
        -- * File-based actions
        fnewE,          -- :: FilePath -> YiM ()
        fwriteE,        -- :: YiM ()
        fwriteAllE,     -- :: YiM ()
        fwriteToE,      -- :: String -> YiM ()
        backupE,        -- :: FilePath -> YiM ()

        -- * Buffer editing
        revertE,        -- :: YiM ()
) where

import Prelude hiding (error)
import Yi.Core
import Yi.Debug
import Yi.Buffer
import Yi.Window
import Yi.Editor
import Yi.Keymap
import qualified Yi.Editor as Editor

import System.Directory     ( doesFileExist, doesDirectoryExist )
import System.FilePath

import Control.Monad.Trans
import Control.Monad.State (gets)
import Control.Monad.Error ()

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> YiM ()
fnewE f = do
    bufs <- withEditor getBuffers
        -- The file names associated with the list of current buffers
    let bufsWithThisFilename = filter ((== Just f) . file) bufs
        -- The names of the existing buffers
        currentBufferNames   = map name bufs
        -- The new name for the buffer
        bufferName           = bestNewName currentBufferNames
    b <- case bufsWithThisFilename of
             [] -> do
                   fe  <- lift $ doesFileExist f
                   de  <- lift $ doesDirectoryExist f
                   newBufferForPath bufferName fe de
             _  -> return (bkey $ head bufsWithThisFilename)
    withGivenBuffer b $ setfileB f        -- associate buffer with file
    withGivenBuffer b $ setSyntaxB (syntaxFromExtension $ takeExtension f)
    switchToBufferE b
    where
    -- The first argument is the buffer name the second argument is
    -- whether or not the file currently exists and the third argument
    -- is whether or not the file is a directory that exists.
    newBufferForPath :: String -> Bool -> Bool -> YiM BufferRef
    newBufferForPath bufferName True _       =
      fileToNewBuffer bufferName f -- Load the file into a new buffer
    newBufferForPath _bufferName False True  =
      do -- Open the dir in Dired
         loadE "Yi.Dired"
         execE $ "Yi.Dired.diredDirBufferE " ++ show f
         withEditor getBuffer
    newBufferForPath bufferName False False  =
      withEditor $ stringToNewBuffer bufferName []  -- Create new empty buffer

    {-
      Working out the name of the syntax from the extension of
      the file. Some of these are a little questionably haskell
      relatex. For example ".x" is an alex lexer specification
      I dare say that there are other file types that use ".x"
      as the file extension.
      For now though this is probably okay given the users of
      'yi' are mostly haskell hackers, as of yet.
    -}
    syntaxFromExtension :: String -> String
    syntaxFromExtension ".hs"    = "haskell"
    syntaxFromExtension ".x"     = "haskell"
    -- anyone want to come up with a literate haskell syntax?
    syntaxFromExtension ".lhs"   = "haskell"
    -- haskell include files such as Yi/Syntax/alex.hsinc
    syntaxFromExtension ".hsinc" = "haskell"
    syntaxFromExtension ".cabal" = "cabal"
    syntaxFromExtension ".tex"   = "latex"
    syntaxFromExtension ".sty"   = "latex"
    syntaxFromExtension ".cxx"   = "cplusplus"
    syntaxFromExtension ".cpp"   = "cplusplus"
    syntaxFromExtension ".h"     = "cplusplus"
    -- I treat c file as cpp files, most users are smart enough
    -- to allow for that.
    syntaxFromExtension ".c"     = "cplusplus"
    -- pepa is a subset of srmc
    syntaxFromExtension ".pepa"  = "srmc"
    syntaxFromExtension ".srmc"  = "srmc"
    syntaxFromExtension _        = "none"

    -- The first argument is the buffer name
    fileToNewBuffer :: String -> FilePath -> YiM BufferRef
    fileToNewBuffer bufferName path = do
      contents <- liftIO $ readFile path
      withEditor $ stringToNewBuffer bufferName contents

    -- Given the desired buffer name, plus a list of current buffer
    -- names returns the best name for the new buffer. This will
    -- be the desired one in the case that it doesn't currently exist.
    -- Otherwise we will suffix it with <n> where n is one more than the
    -- current number of suffixed similar names.
    -- IOW if we want "file.hs" but one already exists then we'll create
    -- "file.hs<1>" but if that already exists then we'll create "file.hs<2>"
    -- and so on.
    desiredBufferName  = takeFileName f
    bestNewName :: [ String ] -> String
    bestNewName currentBufferNames
      | elem desiredBufferName currentBufferNames = addSuffixBName 1
      | otherwise                                 = desiredBufferName
      where
      addSuffixBName :: Int -> String
      addSuffixBName i
        | elem possibleName currentBufferNames = addSuffixBName (i + 1)
        | otherwise                            = possibleName
        where
        possibleName = concat [ desiredBufferName
                              , "<"
                              , show i
                              , ">"
                              ]

-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE = do
            mfp <- withBuffer getfileB
            case mfp of
                     Just fp -> do
                             s <- liftIO $ readFile fp
                             withBuffer $ do
                                  end <- sizeB
                                  p <- pointB
                                  moveTo 0
                                  deleteN end
                                  insertN s
                                  moveTo p
                                  clearUndosB
                             msgE ("Reverted from " ++ show fp)
                     Nothing -> do
                                msgE "Can't revert, no file associated with buffer."
                                return ()

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM ()
fwriteE = do contents <- withBuffer elemsB
             fname <- withBuffer (gets file)
             withBuffer clearUndosB
             case fname of
               Just n -> liftIO $ writeFile n contents
               Nothing -> msgE "Buffer not associated with a file."

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> YiM ()
fwriteToE f = do withBuffer $ setfileB f
                 fwriteE

-- | Write all open buffers
fwriteAllE :: YiM ()
fwriteAllE = error "fwriteAllE not implemented"

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"
