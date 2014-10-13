{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.FuzzyOpen
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This file aims to provide (the essential subset of) the same functionality
-- that vim plugins ctrlp and command-t provide.
--
-- Setup:
--
--   Add something like this to your config:
--
--     (ctrlCh 'p' ?>>! fuzzyOpen)
--
-- Usage:
--
--   <C-p> (or whatever mapping user chooses) starts fuzzy open dialog.
--
--   Typing something filters filelist.
--
--   <Enter> opens currently selected file
--   in current (the one that fuzzyOpen was initited from) window.
--
--   <C-t> opens currently selected file in a new tab.
--   <C-s> opens currently selected file in a split.
--
--   <KUp> and <C-p> moves selection up
--   <KDown> and <C-n> moves selection down
--
--   Readline shortcuts <C-a> , <C-e>, <C-u> and <C-k> work as usual.

module Yi.FuzzyOpen (fuzzyOpen) where

import           Control.Applicative
import           Control.Monad (replicateM, replicateM_, forM, void)
import           Control.Monad.Base
import           Data.Monoid
import qualified Data.Text as T
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath ((</>))
import           Yi
import           Yi.Completion
import           Yi.MiniBuffer
import qualified Yi.Rope as R
import           Yi.Utils ()

fuzzyOpen :: YiM ()
fuzzyOpen = do
    withEditor splitE
    bufRef <- withEditor newTempBufferE
    fileList <- liftBase $ getRecursiveContents "."
    updateMatchList bufRef fileList
    void $ withEditor $ spawnMinibufferE "" $ const $ localKeymap bufRef fileList

-- shamelessly stolen from Chapter 9 of Real World Haskell
-- takes about 3 seconds to traverse linux kernel, which is not too outrageous
-- TODO: check if it works at all with cyclic links
-- TODO: perform in background, limit file count or directory depth
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
          updatingB bufAction = withCurrentBuffer bufAction >> update

showFileList :: [FilePath] -> R.YiString
showFileList = R.fromText . T.intercalate "\n" . map (mappend "  " . T.pack)

{- Implementation detail:
   The index of selected file is stored as vertical cursor position.
   Asterisk position is always synchronized with cursor position.

   TODO: store index of selected file explicitly to make things more obvious.
-}

updateMatchList :: BufferRef -> [FilePath] -> YiM ()
updateMatchList bufRef fileList = do
    needle <- R.toString <$> withCurrentBuffer elemsB
    let filteredFiles = filter (subsequenceMatch needle) fileList
    withEditor $ withGivenBuffer bufRef $ do
        replaceBufferContent $ showFileList filteredFiles
        moveTo 0
        replaceCharB '*'
    return ()

openInThisWindow :: BufferRef -> YiM ()
openInThisWindow = openRoutine (return ())

openInSplit :: BufferRef -> YiM ()
openInSplit = openRoutine splitE

openInNewTab :: BufferRef -> YiM ()
openInNewTab = openRoutine newTabE

openRoutine :: EditorM () -> BufferRef -> YiM ()
openRoutine preOpenAction bufRef = do
  chosenFile <- withEditor $ withGivenBuffer bufRef readLnB
  withEditor $ do
      replicateM_ 2 closeBufferAndWindowE
      preOpenAction
  void . editFile . drop 2 . R.toString $ chosenFile

insertChar :: Keymap
insertChar = textChar >>= write . insertB

moveSelectionUp :: BufferRef -> EditorM ()
moveSelectionUp bufRef = withGivenBuffer bufRef $ do
    replaceCharB ' '
    lineUp
    replaceCharB '*'

moveSelectionDown :: BufferRef -> EditorM ()
moveSelectionDown bufRef = withGivenBuffer bufRef $ do
    replaceCharB ' '
    lineDown
    replaceCharB '*'
