{-# LANGUAGE
  TemplateHaskell,
  ScopedTypeVariables,
  NoMonomorphismRestriction,
  Haskell2010,
  GeneralizedNewtypeDeriving,
  DeriveDataTypeable,
  CPP,
  StandaloneDeriving,
  DeriveGeneric #-}

-- | This module implements persistence across different Yi runs.
--   It includes minibuffer command history, marks etc.
--   Warning: Current version will _not_ check whether two or more instances
--   of Yi are run at the same time.

module Yi.PersistentState(loadPersistentState,
                          savePersistentState,
                          maxHistoryEntries)
where

import Data.Typeable
import Data.Binary
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif
import Data.Default
import System.Directory(doesFileExist)
import qualified Data.Map as M

import Control.Exc(ignoringException)
import Control.Lens

import Yi.Config.Simple.Types(customVariable, Field)
import Yi.Editor
import Yi.History
import Yi.Keymap(YiM)
import Yi.KillRing(Killring(..))
import Yi.Paths(getPersistentStateFilename)
import Yi.Regex(SearchExp(..))
import Yi.Search.Internal (getRegexE, setRegexE)
import Yi.Utils
import Yi.Types (YiConfigVariable)

data PersistentState = PersistentState { histories     :: !Histories
                                       , aKillring     :: !Killring
                                       , aCurrentRegex :: Maybe SearchExp
                                       }

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''PersistentState)
#else
deriving instance Generic PersistentState
instance Binary PersistentState
#endif

newtype MaxHistoryEntries = MaxHistoryEntries { _unMaxHistoryEntries :: Int }
  deriving(Typeable, Binary)

instance Default MaxHistoryEntries where
  def = MaxHistoryEntries 1000

instance YiConfigVariable MaxHistoryEntries

makeLenses ''MaxHistoryEntries

maxHistoryEntries :: Field Int
maxHistoryEntries = customVariable . unMaxHistoryEntries

-- | Trims per-command histories to contain at most N completions each.
trimHistories :: Int -> Histories -> Histories
trimHistories maxHistory (Histories m) = Histories $ M.map trimH m
  where
    trimH (History cur content prefix) = History cur (trim content) prefix
    trim content = drop (max 0 (length content - maxHistory)) content

-- | Here is a persistent history saving part.
--   We assume each command is a single line.
--   To add new components, one has to:
--
--   * add new field in @PersistentState@ structure,
--   * add write and read parts in @loadPersistentState@/@savePersistentState@,
--   * add a trimming code in @savePersistentState@ to prevent blowing up
--     of save file.
savePersistentState :: YiM ()
savePersistentState = do
    MaxHistoryEntries histLimit <- withEditor askConfigVariableA
    pStateFilename      <- getPersistentStateFilename
    (hist :: Histories) <- withEditor $ getEditorDyn
    kr                  <- withEditor $ use killringA
    curRe               <- withEditor   getRegexE
    let pState = PersistentState {
                   histories     = trimHistories histLimit hist
                 , aKillring     = kr    -- trimmed during normal operation
                 , aCurrentRegex = curRe -- just a single value -> no need to trim
                 }
    io $ encodeFile pStateFilename pState

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
loadPersistentState = do
    maybePState <- readPersistentState
    case maybePState of
      Nothing     -> return ()
      Just pState -> do withEditor $ putEditorDyn                $ histories     pState
                        withEditor $ assign killringA            $ aKillring     pState
                        withEditor $ maybe (return ()) setRegexE $ aCurrentRegex pState
