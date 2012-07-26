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

import Control.Exc(ignoringException)

import Yi.Prelude
import Yi.Dynamic
import Yi.History
import Yi.Editor
import Yi.Keymap(YiM)
import Yi.Keymap.Vim.TagStack(VimTagStack(..), getTagStack, setTagStack)
import Yi.KillRing(Killring(..))
import Yi.Search(getRegexE, setRegexE)
import Yi.Regex(SearchExp(..))


data PersistentState = PersistentState { histories     :: !Histories
                                       , vimTagStack   :: !VimTagStack
                                       , aKillring     :: !Killring
                                       , aCurrentRegex :: Maybe SearchExp
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

savePersistentState :: YiM ()
savePersistentState = do pStateFilename <- getPersistentStateFilename
                         (hist :: Histories) <- withEditor $ getA dynA
                         tagStack            <- withEditor $ getTagStack
                         kr                  <- withEditor $ getA killringA
                         curRe               <- withEditor $ getRegexE
                         let pState = PersistentState { histories     = M.map trimH hist
                                                      , vimTagStack   = tagStack
                                                      , aKillring     = kr
                                                      , aCurrentRegex = curRe
                                                      }
                         io $ encodeFile pStateFilename $ pState
  where
    maxHistory = 100 -- TODO: make configurable
    trimH (History cur content prefix) = History cur (trim content) prefix
    trim content = drop (max 0 (length content - maxHistory)) content

readPersistentState :: YiM (Maybe PersistentState)
readPersistentState = do pStateFilename <- getPersistentStateFilename
                         pStateExists <- io $ doesFileExist pStateFilename
                         if not pStateExists
                           then return Nothing
                           else io $ ignoringException $ strictDecoder pStateFilename
  where
    strictDecoder filename = do (state :: PersistentState) <- decodeFile filename
                                state `seq` return (Just state)

loadPersistentState :: YiM ()
loadPersistentState = do maybePState <- readPersistentState
                         case maybePState of
                           Nothing     -> return ()
                           Just pState -> do withEditor $ putA dynA                   $ histories     pState
                                             withEditor $ setTagStack                 $ vimTagStack   pState
                                             withEditor $ putA killringA              $ aKillring     pState
                                             withEditor $ maybe (return ()) setRegexE $ aCurrentRegex pState

