{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, Haskell2010, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
-- Copyright '2012 by Michal J. Gajda
--
-- | This module implements persistence across different Yi runs.
--   It includes minibuffer command history, marks, VimTagStack etc.
--   Warning: Current version will _not_ check whether two or more instances
--   of Yi are run at the same time.

module Yi.PersistentState(loadPersistentState,
                          savePersistentState,
                          maxHistoryEntries)
where

import Prelude hiding ((.))
import Data.Binary
import Data.DeriveTH
import Data.Accessor.Template(nameDeriveAccessors)
import System.Directory(doesFileExist)
import qualified Data.Map as M

import Control.Exc(ignoringException)

import Yi.Prelude
import Yi.Dynamic
import Yi.Config.Simple.Types(customVariable, Field)
import Yi.History
import Yi.Editor
import Yi.Keymap(YiM)
import Yi.Keymap.Vim.TagStack(VimTagStack(..), getTagStack, setTagStack)
import Yi.KillRing(Killring(..))
import Yi.Search(getRegexE, setRegexE)
import Yi.Regex(SearchExp(..))
import Yi.Paths(getPersistentStateFilename)


data PersistentState = PersistentState { histories     :: !Histories
                                       , vimTagStack   :: !VimTagStack
                                       , aKillring     :: !Killring
                                       , aCurrentRegex :: Maybe SearchExp
                                       }

$(derive makeBinary ''PersistentState)

newtype MaxHistoryEntries = MaxHistoryEntries { unMaxHistoryEntries :: Int }
  deriving(Typeable, Binary)

instance Initializable MaxHistoryEntries where
  initial = MaxHistoryEntries 1000

instance YiConfigVariable MaxHistoryEntries

$(nameDeriveAccessors ''MaxHistoryEntries (\n -> Just (n ++ "A")))

maxHistoryEntries :: Field Int
maxHistoryEntries = unMaxHistoryEntriesA . customVariable

-- | Trims per-command histories to contain at most N completions each.
trimHistories :: Int -> Histories -> Histories
trimHistories maxHistory = M.map trimH
  where
    trimH (History cur content prefix) = History cur (trim content) prefix
    trim content = drop (max 0 (length content - maxHistory)) content

-- | Trims VimTagStack to contain at most N values.
trimTagStack :: Int -> VimTagStack -> VimTagStack
trimTagStack maxHistory = VimTagStack . take maxHistory . tagsStack

-- | Here is a persistent history saving part.
--   We assume each command is a single line.
--   To add new components, one has to:
--
--   * add new field in @PersistentState@ structure, 
--   * add write and read parts in @loadPersistentState@/@savePersistentState@,
--   * add a trimming code in @savePersistentState@ to prevent blowing up
--     of save file.
savePersistentState :: YiM ()
savePersistentState = do MaxHistoryEntries histLimit <- withEditor $ askConfigVariableA
                         pStateFilename      <- getPersistentStateFilename
                         (hist :: Histories) <- withEditor $ getA dynA
                         tagStack            <- withEditor $ getTagStack
                         kr                  <- withEditor $ getA killringA
                         curRe               <- withEditor $ getRegexE
                         let pState = PersistentState { histories     = trimHistories histLimit hist
                                                      , vimTagStack   = trimTagStack  histLimit tagStack
                                                      , aKillring     = kr    -- trimmed during normal operation
                                                      , aCurrentRegex = curRe -- just a single value -> no need to trim
                                                      }
                         io $ encodeFile pStateFilename $ pState

-- | Reads and decodes a persistent state in both strict, and exception robust
--   way.
readPersistentState :: YiM (Maybe PersistentState)
readPersistentState = do pStateFilename <- getPersistentStateFilename
                         pStateExists <- io $ doesFileExist pStateFilename
                         if not pStateExists
                           then return Nothing
                           else io $ ignoringException $ strictDecoder pStateFilename
  where
    strictDecoder filename = do (state :: PersistentState) <- decodeFile filename
                                state `seq` return (Just state)

-- | Loads a persistent state, and sets Yi state variables accordingly.
loadPersistentState :: YiM ()
loadPersistentState = do maybePState <- readPersistentState
                         case maybePState of
                           Nothing     -> return ()
                           Just pState -> do withEditor $ putA dynA                   $ histories     pState
                                             withEditor $ setTagStack                 $ vimTagStack   pState
                                             withEditor $ putA killringA              $ aKillring     pState
                                             withEditor $ maybe (return ()) setRegexE $ aCurrentRegex pState

