{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Yi.Keymap.Vim2.Common
    ( VimMode(..)
    , VimBinding(..)
    , GotoCharCommand(..)
    , VimState(..)
    , Register(..)
    , RepeatToken(..)
    , RepeatableAction(..)
    , MatchResult(..)
    , EventString
    , OperatorName
    , RegisterName
    , module Yi.Keymap.Vim2.MatchResult
    ) where

import Data.Binary
import Data.DeriveTH
import qualified Data.HashMap.Strict as HM
import qualified Data.Rope as R
import Data.Default
import Data.Typeable

import Yi.Buffer hiding (Insert)
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.MatchResult

type EventString = String

type OperatorName = String

type RegisterName = Char

data RepeatableAction = RepeatableAction {
          raPreviousCount :: !Int
        , raActionString :: !EventString
    }
    deriving (Typeable, Eq, Show)

data Register = Register {
          regRegionStyle :: RegionStyle
        , regContent :: R.Rope
    }

data VimMode = Normal
             | NormalOperatorPending OperatorName
             | Insert Char -- ^ char denotes how state got into insert mode ('i', 'a', etc.)
             | Replace
             | ReplaceSingleChar
             | InsertNormal -- ^ after C-o
             | InsertVisual -- ^ after C-o and one of v, V, C-v
             | Visual RegionStyle
             | Ex
             | Search { previousMode :: VimMode, direction :: Direction }
    deriving (Typeable, Eq, Show)

data GotoCharCommand = GotoCharCommand !Char !Direction !RegionStyle

data VimState = VimState {
          vsMode :: !VimMode
        , vsCount :: !(Maybe Int)
        , vsAccumulator :: !EventString -- ^ for repeat and potentially macros
        , vsTextObjectAccumulator :: !EventString
        , vsRegisterMap :: !(HM.HashMap RegisterName Register)
        , vsActiveRegister :: !RegisterName
        , vsRepeatableAction :: !(Maybe RepeatableAction)
        , vsStringToEval :: !EventString -- ^ see Yi.Keymap.Vim2.vimEval comment
        , vsStickyEol :: !Bool -- ^ is set on $, allows j and k walk the right edge of lines
        , vsOngoingInsertEvents :: !EventString
        , vsLastGotoCharCommand :: !(Maybe GotoCharCommand)
        , vsBindingAccumulator :: !EventString
        , vsSecondaryCursors :: ![Point]
        , vsPaste :: !Bool -- ^ like vim's :help paste
    } deriving (Typeable)

$(derive makeBinary ''RepeatableAction)

$(derive makeBinary ''Register)

$(derive makeBinary ''GotoCharCommand)

instance Default VimMode where
    def = Normal

$(derive makeBinary ''VimMode)

instance Default VimState where
    def = VimState Normal Nothing [] [] HM.empty '\0' Nothing [] False [] Nothing [] [] False

$(derive makeBinary ''VimState)

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
data VimBinding = VimBindingY {
                      vbPrerequisite :: EventString -> VimState -> MatchResult (),
                      vbyAction :: EventString -> YiM RepeatToken
                  }
                | VimBindingE {
                      vbPrerequisite :: EventString -> VimState -> MatchResult (),
                      vbeAction :: EventString -> EditorM RepeatToken
                  }
