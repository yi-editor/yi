{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Yi.Keymap.Vim2.Common
    ( VimMode(..)
    , VimBinding(..)
    , VimState(..)
    , VimMotion(..)
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

data VimMode = Normal
             | NormalOperatorPending
             | Insert
             | Replace
             | InsertNormal -- ^ after C-o
             | InsertVisual -- ^ after C-o and one of v, V, C-v
             | Visual RegionStyle
             | CmdLine
    deriving (Typeable, Eq)

data VimState = VimState {
        vsMode :: !VimMode,
        vsCount :: !(Maybe Int)
    } deriving (Typeable)

instance Initializable VimMode where
    initial = Normal

$(derive makeBinary ''VimMode)

instance Initializable VimState where
    initial = VimState Normal Nothing

$(derive makeBinary ''VimState)

instance YiVariable VimState

-- Distinction between YiM and EditorM variants is for testing.
data VimBinding = VimBindingY {
                      vbyPrerequisite :: Event -> VimState -> Bool,
                      vbyAction :: Event -> YiM ()
                  }
                | VimBindingE {
                      vbePrerequisite :: Event -> VimState -> Bool,
                      vbeAction :: Event -> EditorM ()
                  }

data Operator = Operator

data VimMotion = VMChar Direction
               | VMLine Direction
               | VMWordStart Direction
               | VMWordEnd Direction
               | VMWORDStart Direction
               | VMWORDEnd Direction
               | VMSOL
               | VMNonEmptySOL
               | VMEOL
