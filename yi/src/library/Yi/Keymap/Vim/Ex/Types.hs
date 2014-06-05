module Yi.Keymap.Vim.Ex.Types where

import Data.Maybe

import Yi.Keymap

data ExCommand = ExCommand {
    cmdComplete :: YiM [String]
  , cmdIsPure :: Bool
  , cmdAction :: Action
  , cmdAcceptsRange :: Bool
  , cmdShow :: String
}

instance Show ExCommand where
    show = cmdShow

data LineRange
    = MarkRange String String -- ^ 'a,'b
    | FullRange               -- ^ %
    | CurrentLineRange

stringToExCommand :: [String -> Maybe ExCommand] -> String -> Maybe ExCommand
stringToExCommand parsers s = listToMaybe . mapMaybe ($ s) $ parsers
