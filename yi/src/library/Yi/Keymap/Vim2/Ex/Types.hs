{-# LANGUAGE ExistentialQuantification #-}

module Yi.Keymap.Vim2.Ex.Types where

import Prelude ()
import Yi.Prelude

import Data.Maybe

import Yi.Editor
import Yi.Keymap

class Show e => ExCommand e where
    cmdParse :: e -> String -> Maybe e
    cmdComplete :: e -> YiM (Maybe String)
    cmdIsPure :: e -> Bool
    cmdAction :: e -> Either (EditorM ()) (YiM ())

data ExCommandBox = forall a. ExCommand a => ExCommandBox a

pack :: forall a. ExCommand a => a -> ExCommandBox
pack = ExCommandBox

instance ExCommand ExCommandBox where
    cmdParse (ExCommandBox e) = fmap ExCommandBox . cmdParse e
    cmdComplete (ExCommandBox e) = cmdComplete e
    cmdIsPure (ExCommandBox e) = cmdIsPure e
    cmdAction (ExCommandBox e) = cmdAction e

instance Show ExCommandBox where
    show (ExCommandBox e) = show e

stringToExCommand :: [ExCommandBox] -> String -> Maybe ExCommandBox
stringToExCommand cmds s = listToMaybe . mapMaybe (`cmdParse` s) $ cmds
