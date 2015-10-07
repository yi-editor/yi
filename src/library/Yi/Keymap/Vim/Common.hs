{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Common
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Common types used by the vim keymap.

module Yi.Keymap.Vim.Common
    ( VimMode(..)
    , VimBinding(..)
    , GotoCharCommand(..)
    , VimState(..)
    , Register(..)
    , RepeatToken(..)
    , RepeatableAction(..)
    , MatchResult(..)
    , EventString(..), unEv
    , OperatorName(..), unOp
    , RegisterName
    , module Yi.Keymap.Vim.MatchResult
    , lookupBestMatch, matchesString
    ) where

import           GHC.Generics              (Generic)

import           Control.Applicative       (Alternative ((<|>)), (<$>))
import           Control.Lens              (makeLenses)
import           Data.Binary               (Binary (..))
import           Data.Default              (Default (..))
import qualified Data.HashMap.Strict       as HM (HashMap)
import           Data.Monoid               (Monoid (mappend, mempty), (<>))
import           Data.String               (IsString (..))
import qualified Data.Text                 as T (Text, isPrefixOf, pack)
import qualified Data.Text.Encoding        as E (decodeUtf8, encodeUtf8)
import           Data.Typeable             (Typeable)
import           Yi.Buffer.Adjusted        (Direction, Point, RegionStyle)
import           Yi.Editor                 (EditorM)
import           Yi.Keymap                 (YiM)
import           Yi.Keymap.Vim.MatchResult (MatchResult (..))
import           Yi.Rope                   (YiString)
import           Yi.Types                  (YiVariable)


newtype EventString = Ev { _unEv :: T.Text } deriving (Show, Eq, Ord)

instance IsString EventString where
  fromString = Ev . T.pack

newtype OperatorName = Op { _unOp :: T.Text } deriving (Show, Eq)

instance IsString OperatorName where
  fromString = Op . T.pack

instance Monoid EventString where
  mempty = Ev mempty
  Ev t `mappend` Ev t' = Ev $ t <> t'

instance Monoid OperatorName where
  mempty = Op mempty
  Op t `mappend` Op t' = Op $ t <> t'

instance Binary EventString where
  get = Ev . E.decodeUtf8 <$> get
  put (Ev t) = put $ E.encodeUtf8 t

instance Binary OperatorName where
  get = Op . E.decodeUtf8 <$> get
  put (Op t) = put $ E.encodeUtf8 t

makeLenses ''EventString
makeLenses ''OperatorName

-- 'lookupBestMatch' and 'matchesString' pulled out of MatchResult
-- module to prevent cyclic dependencies. Screw more bootfiles.
lookupBestMatch :: EventString -> [(EventString, a)] -> MatchResult a
lookupBestMatch key = foldl go NoMatch
    where go m (k, x) = m <|> fmap (const x) (key `matchesString` k)

matchesString :: EventString -> EventString -> MatchResult ()
matchesString (Ev got) (Ev expected)
  | expected == got = WholeMatch ()
  | got `T.isPrefixOf` expected = PartialMatch
  | otherwise = NoMatch

type RegisterName = Char
type MacroName = Char

data RepeatableAction = RepeatableAction
    { raPreviousCount :: !Int
    , raActionString  :: !EventString
    } deriving (Typeable, Eq, Show, Generic)

data Register = Register
    { regRegionStyle :: RegionStyle
    , regContent     :: YiString
    } deriving (Generic)

data VimMode
    = Normal
    | NormalOperatorPending OperatorName
    | Insert Char -- ^ char denotes how state got into insert mode ('i', 'a', etc.)
    | Replace
    | ReplaceSingleChar
    | InsertNormal -- ^ after C-o
    | InsertVisual -- ^ after C-o and one of v, V, C-v
    | Visual RegionStyle
    | Ex
    | Search { previousMode :: VimMode, direction :: Direction }
    deriving (Typeable, Eq, Show, Generic)

data GotoCharCommand = GotoCharCommand !Char !Direction !RegionStyle
    deriving (Generic)

data VimState = VimState
    { vsMode                  :: !VimMode
    , vsCount                 :: !(Maybe Int)
    , vsAccumulator           :: !EventString -- ^ for repeat and potentially macros
    , vsTextObjectAccumulator :: !EventString
    , vsRegisterMap           :: !(HM.HashMap RegisterName Register)
    , vsActiveRegister        :: !RegisterName
    , vsRepeatableAction      :: !(Maybe RepeatableAction)
    , vsStringToEval          :: !EventString -- ^ see Yi.Keymap.Vim.vimEval comment
    , vsOngoingInsertEvents   :: !EventString
    , vsLastGotoCharCommand   :: !(Maybe GotoCharCommand)
    , vsBindingAccumulator    :: !EventString
    , vsSecondaryCursors      :: ![Point]
    , vsPaste                 :: !Bool -- ^ like vim's :help paste
    , vsCurrentMacroRecording :: !(Maybe (MacroName, EventString))
    } deriving (Typeable, Generic)

instance Binary RepeatableAction
instance Binary Register
instance Binary GotoCharCommand

instance Default VimMode where
    def = Normal

instance Binary VimMode

instance Default VimState where
    def = VimState
            Normal -- mode
            Nothing -- count
            mempty -- accumulator
            mempty -- textobject accumulator
            mempty -- register map
            '\0' -- active register
            Nothing -- repeatable action
            mempty -- string to eval
            mempty -- ongoing insert events
            Nothing -- last goto char command
            mempty -- binding accumulator
            mempty -- secondary cursors
            False -- :set paste
            Nothing -- current macro recording

instance Binary VimState

instance YiVariable VimState

-- Whether an action can be repeated through the use of the '.' key.
--
-- Actions with a RepeatToken of:
--
--  - Finish are repeatable.
--  - Drop are not repeatable.
--  - Continue are currently in progress. They will become repeatable when
--    completed. It is possible to cancel a in progress action, in which case
--    it will not be repeatable.
data RepeatToken = Finish
                 | Drop
                 | Continue
    deriving Show

-- Distinction between YiM and EditorM variants is for testing.
data VimBinding
    = VimBindingY (EventString -> VimState -> MatchResult (YiM RepeatToken))
    | VimBindingE (EventString -> VimState -> MatchResult (EditorM RepeatToken))
