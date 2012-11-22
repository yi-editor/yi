{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Yi.Keymap.Vim2.Common
    ( VimMode(..)
    , VimBinding(..)
    , VimState(..)
    , VimMotion(..)
    , VimOperator(..)
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
        , regContent :: Maybe R.Rope
    }

data VimMode = Normal
             | NormalOperatorPending VimOperator
             | Insert
             | Replace
             | ReplaceSingleChar
             | InsertNormal -- ^ after C-o
             | InsertVisual -- ^ after C-o and one of v, V, C-v
             | Visual RegionStyle
             | CmdLine
    deriving (Typeable, Eq, Show)

data VimState = VimState {
          vsMode :: !VimMode
        , vsCount :: !(Maybe Int)
        , vsAccumulator :: !String -- ^ for repeat and potentially macros
        , vsTextObjectAccumulator :: !String
        , vsRegisterMap :: !(Maybe (HM.HashMap Char Register))
        , vsRepeatableAction :: !(Maybe RepeatableAction)
        , vsStringToEval :: !String -- ^ see Yi.Keymap.Vim2.vimEval comment
    } deriving (Typeable)

$(derive makeBinary ''VimOperator)

$(derive makeBinary ''RepeatableAction)

$(derive makeBinary ''Register)

instance Initializable VimMode where
    initial = Normal

$(derive makeBinary ''VimMode)

instance Initializable VimState where
    initial = VimState Normal Nothing [] [] Nothing Nothing []

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

data VimMotion = VMChar Direction
               | VMLine Direction
               | VMWordStart Direction
               | VMWordEnd Direction
               | VMWORDStart Direction
               | VMWORDEnd Direction
               | VMSOL
               | VMNonEmptySOL
               | VMEOL

