{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, Haskell2010 #-}
-- Copyright '2012 by Michal J. Gajda
--
-- | This module implements persistence across different Yi runs.
--   It includes minibuffer command history, marks, VimTagStack etc.
--   Warning: Current version will _not_ check whether two or more instances
--   of Yi are run at the same time.

module Yi.PersistentState(loadPersistentState,
                          savePersistentState)
where

import Data.Binary
import Data.DeriveTH
import System.FilePath((</>))
import System.Directory(getAppUserDataDirectory, doesFileExist)
import qualified Data.Map as M

import Yi.Prelude
import Yi.Dynamic
import Yi.History
import Yi.Editor
import Yi.Keymap(YiM)
import Yi.Keymap.Vim.TagStack(VimTagStack(..), getTagStack, setTagStack)

data PersistentState = PersistentState { histories   :: Histories
                                       , vimTagStack :: VimTagStack
                                       }

$(derive makeBinary ''PersistentState)

-- | Here is a persistent history saving part.
--   We assume each command is a single line.
--   To add new components, one has to:
--
--   * add new field in @PersistentState@ structure, 
--   * add write and read parts in @loadPersistentState@/@savePersistentState@,
--   * add a trimming code in @savePersistentState@ to prevent blowing up
--     of save file.
--
-- TODO: trim contents by amount set by config variable

getPersistentStateFilename :: YiM String
getPersistentStateFilename = do cfgDir <- io $ getAppUserDataDirectory "yi"
                                return $ cfgDir </> "history"

loadPersistentState, savePersistentState :: YiM ()
savePersistentState = do pStateFilename <- getPersistentStateFilename
                         (hist :: Histories) <- withEditor $ getA dynA
                         tagStack <- withEditor $ getTagStack
                         let pState = PersistentState { histories   = M.map trimH hist
                                                      , vimTagStack = tagStack
                                                      }
                         io $ encodeFile pStateFilename $ pState
  where
    maxHistory = 100 -- TODO: make configurable
    trimH (History cur content prefix) = History cur (trim content) prefix
    trim content = drop (max 0 (length content - maxHistory)) content

loadPersistentState = do pStateFilename <- getPersistentStateFilename
                         pStateExists <- io $ doesFileExist pStateFilename
                         when pStateExists $
                           do (pState :: PersistentState) <- io $ decodeFile pStateFilename
                              withEditor $ putA dynA   $ histories   pState
                              withEditor $ setTagStack $ vimTagStack pState

