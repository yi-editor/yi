module Yi.FuzzyOpen
    ( fuzzyOpen
    ) where

import Prelude ()
import Yi
import Yi.MiniBuffer
import Yi.Completion

import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (FilePath, (</>))
import Data.List (filter, map, intersperse, drop)
import Data.Maybe (isJust)

fuzzyOpen :: YiM ()
fuzzyOpen = do
    withEditor splitE
    bufRef <- withEditor newTempBufferE
    fileList <- liftIO $ getRecursiveContents "."
    updateMatchList bufRef fileList
    withEditor $ spawnMinibufferE "" $ const $ localKeymap bufRef fileList
    return ()

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".git", ".svn"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)

localKeymap :: BufferRef -> [FilePath] -> Keymap
localKeymap bufRef fileList =
              choice [spec KEnter  ?>>! openInThisWindow bufRef
                     , ctrlCh 't'  ?>>! openInNewTab bufRef
                     , ctrlCh 's'  ?>>! openInSplit bufRef
                     , spec KEsc   ?>>! replicateM 2 closeBufferAndWindowE
                     , ctrlCh 'h'  ?>>! updatingB (deleteB Character Backward)
                     , spec KBS    ?>>! updatingB (deleteB Character Backward)
                     , spec KDel   ?>>! updatingB (deleteB Character Backward)
                     , ctrlCh 'a'  ?>>! moveToSol
                     , ctrlCh 'e'  ?>>! moveToEol
                     , spec KLeft  ?>>! moveXorSol 1
                     , spec KRight ?>>! moveXorEol 1
                     , ctrlCh 'p'  ?>>! moveSelectionUp bufRef
                     , spec KUp    ?>>! moveSelectionUp bufRef
                     , ctrlCh 'n'  ?>>! moveSelectionDown bufRef
                     , spec KDown  ?>>! moveSelectionDown bufRef
                     , ctrlCh 'w'  ?>>! updatingB (deleteB unitWord Backward)
                     , ctrlCh 'u'  ?>>! updatingB (moveToSol >> deleteToEol)
                     , ctrlCh 'k'  ?>>! updatingB deleteToEol
                ]
             <|| (insertChar >>! update)
    where update = updateMatchList bufRef fileList
          updatingB bufAction = withBuffer bufAction >> update
          updatingE editorAction = withEditor editorAction >> update

insertChar :: Keymap
insertChar = textChar >>= write . insertB

showFileList :: [FilePath] -> String
showFileList = concat . intersperse "\n" . map ("  " ++)

updateMatchList :: BufferRef -> [FilePath] -> YiM ()
updateMatchList bufRef fileList = do
    needle <- withBuffer elemsB
    let filteredFiles = filter (isJust . subsequenceMatch needle) fileList
    withEditor $ withGivenBuffer0 bufRef $ do
        replaceBufferContent $ showFileList filteredFiles
        moveTo 0
        replaceCurrentChar '*'
    return ()

replaceCurrentChar :: Char -> BufferM ()
replaceCurrentChar c = do
    deleteN 1
    insertB c
    leftB

openInThisWindow :: BufferRef -> YiM ()
openInThisWindow = openRoutine (return ())

openInSplit :: BufferRef -> YiM ()
openInSplit = openRoutine splitE

openInNewTab :: BufferRef -> YiM ()
openInNewTab = openRoutine newTabE

openRoutine :: EditorM () -> BufferRef -> YiM ()
openRoutine preOpenAction bufRef = do
    chosenFile <- fmap (drop 2) $ withEditor $ withGivenBuffer0 bufRef $ do
        moveTo 0
        getLine
    withEditor $ do
        replicateM 2 closeBufferAndWindowE
        preOpenAction
    discard $ editFile chosenFile

getLine :: BufferM String
getLine = do
    moveToSol
    p0 <- pointB
    moveToEol
    p1 <- pointB
    nelemsB (fromPoint p1 - fromPoint p0) p0

-- | Parse any character that can be inserted in the text.
textChar :: KeymapM Char
textChar = do
  Event (KASCII c) [] <- anyEvent
  return c

moveSelectionUp :: BufferRef -> EditorM () 
moveSelectionUp bufRef = withGivenBuffer0 bufRef $ do
    replaceCurrentChar ' '
    lineUp
    replaceCurrentChar '*'
    
moveSelectionDown :: BufferRef -> EditorM () 
moveSelectionDown bufRef = withGivenBuffer0 bufRef $ do
    replaceCurrentChar ' '
    lineDown
    replaceCurrentChar '*'
