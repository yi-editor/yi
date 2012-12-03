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
import Yi.Event
import Yi.Keymap

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
        , raActionString :: !String
    }
    deriving (Typeable, Eq, Show)

data Register = Register {
          regRegionStyle :: RegionStyle
        , regContent :: R.Rope
    }

data VimMode = Normal
             | NormalGotoCharacter Direction RegionStyle
             | NormalOperatorPending VimOperator
             | Insert
             | Replace
             | ReplaceSingleChar
             | InsertNormal -- ^ after C-o
             | InsertVisual -- ^ after C-o and one of v, V, C-v
             | Visual RegionStyle
             | CmdLine
    deriving (Typeable, Eq, Show)

data GotoCharCommand = GotoCharCommand !Char !Direction !RegionStyle

data VimState = VimState {
          vsMode :: !VimMode
        , vsCount :: !(Maybe Int)
        , vsAccumulator :: !String -- ^ for repeat and potentially macros
        , vsTextObjectAccumulator :: !String
        , vsRegisterMap :: !(HM.HashMap Char Register)
        , vsRepeatableAction :: !(Maybe RepeatableAction)
        , vsStringToEval :: !String -- ^ see Yi.Keymap.Vim2.vimEval comment
        , vsStickyEol :: !Bool -- ^ is set on $, allows j and k walk the right edge of lines
        , vsOngoingInsertEvents :: !String
        , vsLastGotoCharCommand :: !(Maybe GotoCharCommand)
    } deriving (Typeable)

$(derive makeBinary ''VimOperator)

$(derive makeBinary ''RepeatableAction)

$(derive makeBinary ''Register)

$(derive makeBinary ''GotoCharCommand)

instance Initializable VimMode where
    initial = Normal

$(derive makeBinary ''VimMode)

instance Initializable VimState where
    initial = VimState Normal Nothing [] [] HM.empty Nothing [] False [] Nothing

$(derive makeBinary ''VimState)

instance YiVariable VimState

-- TODO: explain
data RepeatToken = Finish
                 | Drop
                 | Continue
    deriving Show

-- Distinction between YiM and EditorM variants is for testing.
data VimBinding = VimBindingY {
                      vbPrerequisite :: Event -> VimState -> Bool,
                      vbyAction :: Event -> YiM RepeatToken
                  }
                | VimBindingE {
                      vbPrerequisite :: Event -> VimState -> Bool,
                      vbeAction :: Event -> EditorM RepeatToken
                  }
