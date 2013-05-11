module Yi.Keymap.Vim2.Ex.Types where

import Prelude ()
import Yi.Prelude

import System.FilePath

data ExReplaceFlag = Global
                   | CaseInsensitive
                   | EveryLine
    deriving (Show, Eq)

data ExPureCommand = ExGlobal String Bool ExPureCommand
                   | ExDelete
                   | ExReplace String String [ExReplaceFlag]
                   | ExGotoLine Int
    deriving (Show, Eq)

data ExImpureCommand = ExEdit FilePath
                     | ExTabedit FilePath
                     | ExQuit Bool
                     | ExQuitAll Bool
    deriving (Show, Eq)

data ExCommand = ExPure ExPureCommand
               | ExImpure ExImpureCommand
    deriving (Show, Eq)
