{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Yi.Keymap.Vim2.Common
    ( VimMode(..)
    , VimBinding(..)
    , GotoCharCommand(..)
    , VimState(..)
    , VimOperator(..)
    , Register(..)
    , RepeatToken(..)
    , RepeatableAction(..)
    , MatchResult(..)
    , EventString
    ) where

import Yi.Prelude
import Prelude ()

import Data.Binary
import Data.DeriveTH
import qualified Data.HashMap.Strict as HM
import qualified Data.Rope as R

import Yi.Buffer hiding (Insert)
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap

type EventString = String

data VimOperator = OpYank
                 | OpDelete
                 | OpChange
                 | OpSwitchCase
                 | OpUpperCase
                 | OpLowerCase
                 | OpReindent
                 | OpShiftRight
                 | OpShiftLeft
                 | OpRot13
                 | OpFormat
    deriving (Typeable, Eq, Show)

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
             | NormalOperatorPending VimOperator
             | Insert Char -- ^ char denotes how state got into insert mode ('i', 'a', etc.)
             | Replace
             | ReplaceSingleChar
             | InsertNormal -- ^ after C-o
             | InsertVisual -- ^ after C-o and one of v, V, C-v
             | Visual RegionStyle
             | Ex
             | Search { searchCommand :: String, previousMode :: VimMode, direction :: Direction }
    deriving (Typeable, Eq, Show)

data GotoCharCommand = GotoCharCommand !Char !Direction !RegionStyle

data VimState = VimState {
          vsMode :: !VimMode
        , vsCount :: !(Maybe Int)
        , vsAccumulator :: !EventString -- ^ for repeat and potentially macros
        , vsTextObjectAccumulator :: !EventString
        , vsRegisterMap :: !(HM.HashMap Char Register)
        , vsRepeatableAction :: !(Maybe RepeatableAction)
        , vsStringToEval :: !EventString -- ^ see Yi.Keymap.Vim2.vimEval comment
        , vsStickyEol :: !Bool -- ^ is set on $, allows j and k walk the right edge of lines
        , vsOngoingInsertEvents :: !EventString
        , vsLastGotoCharCommand :: !(Maybe GotoCharCommand)
        , vsBindingAccumulator :: !EventString
        , vsSecondaryCursors :: ![Point]
    } deriving (Typeable)

$(derive makeBinary ''VimOperator)

$(derive makeBinary ''RepeatableAction)

$(derive makeBinary ''Register)

$(derive makeBinary ''GotoCharCommand)

instance Initializable VimMode where
    initial = Normal

$(derive makeBinary ''VimMode)

instance Initializable VimState where
    initial = VimState Normal Nothing [] [] HM.empty Nothing [] False [] Nothing [] []

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

data MatchResult a = NoMatch
                   | PartialMatch
                   | WholeMatch a

instance Functor MatchResult where
    fmap f (WholeMatch x) = WholeMatch (f x)
    fmap _ NoMatch = NoMatch
    fmap _ PartialMatch = PartialMatch

instance Applicative MatchResult where
    pure = WholeMatch
    WholeMatch f <*> WholeMatch x = WholeMatch (f x)
    _ <*> _ = NoMatch

instance Alternative MatchResult where
    empty = NoMatch
    WholeMatch x <|> _ = WholeMatch x
    _ <|> WholeMatch x = WholeMatch x
    PartialMatch <|> _ = PartialMatch
    _ <|> PartialMatch = PartialMatch
    _ <|> _ = NoMatch

instance Show (MatchResult a) where
    show (WholeMatch _) = "WholeMatch"
    show PartialMatch = "PartialMatch"
    show NoMatch = "NoMatch"

-- Distinction between YiM and EditorM variants is for testing.
data VimBinding = VimBindingY {
                      vbPrerequisite :: EventString -> VimState -> MatchResult (),
                      vbyAction :: EventString -> YiM RepeatToken
                  }
                | VimBindingE {
                      vbPrerequisite :: EventString -> VimState -> MatchResult (),
                      vbeAction :: EventString -> EditorM RepeatToken
                  }
