{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Yi.Keymap.Vim2.Common
    ( VimMode(..)
    , VimBinding(..)
    , VimState(..)
    , VimMotion(..)
    , RepeatableAction(..)
    ) where

import Yi.Prelude
import Prelude ()

import Data.Binary
import Data.DeriveTH

import Yi.Buffer hiding (Insert)
import Yi.Dynamic
import Yi.Editor
import Yi.Event
import Yi.Keymap

data Operator = OpYank
              | OpDelete
              | OpChange
              | OpSwitchCase
              | OpToUpperCase
              | OpToLowerCase
    deriving (Typeable, Eq, Show)

data RepeatableAction = RepeatableAction {
          raPreviousCount :: !Int
        , raActionString :: !String
    }
    deriving (Typeable, Eq)

data VimMode = Normal
             | NormalOperatorPending Operator
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
        , vsAccumulator :: !String
        , vsRepeatableAction :: !(Maybe RepeatableAction)
        , vsStringToEval :: !String -- ^ see Yi.Keymap.Vim2.vimEval comment
    } deriving (Typeable)

$(derive makeBinary ''Operator)

$(derive makeBinary ''RepeatableAction)

instance Initializable VimMode where
    initial = Normal

$(derive makeBinary ''VimMode)

instance Initializable VimState where
    initial = VimState Normal Nothing [] Nothing []

$(derive makeBinary ''VimState)

instance YiVariable VimState

-- Distinction between YiM and EditorM variants is for testing.
data VimBinding = VimBindingY {
                      vbPrerequisite :: Event -> VimState -> Bool,
                      vbyAction :: Event -> YiM ()
                  }
                | VimBindingE {
                      vbPrerequisite :: Event -> VimState -> Bool,
                      vbeAction :: Event -> EditorM ()
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

