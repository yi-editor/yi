{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Templates
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Templates for inserting into documents

module Yi.Templates ( templates
                    , templateNames
                    , lookupTemplate
                    , addTemplate
                    ) where

import qualified Data.Map as Map
import           Yi.Buffer (BufferM, insertN)
import           Yi.Editor (EditorM, withCurrentBuffer, printMsg)
import qualified Yi.Rope as R

type Template       = String
type TemplateName   = String
type TemplateLookup = Map.Map TemplateName Template

addTemplate :: String -> EditorM ()
addTemplate tName =
  case lookupTemplate tName of
    Nothing -> printMsg "template-name not found"
    Just t  -> withCurrentBuffer $ addTemplateBuffer t
  where
  addTemplateBuffer :: Template -> BufferM ()
  addTemplateBuffer = insertN . R.fromString

templates :: TemplateLookup
templates =
  Map.fromList $ concat [ haskellTemplates ]

templateNames :: [ TemplateName ]
templateNames = Map.keys templates


lookupTemplate :: TemplateName -> Maybe Template
lookupTemplate name = Map.lookup name templates


haskellTemplates :: [ (TemplateName, Template) ]
haskellTemplates =
  [ ( "haskell-module"
    , unlines [ "module "
              , "  ()"
              , "where"
              , ""
              , "{- Standard Library Modules Imported -}"
              , "{- External Library Modules Imported -}"
              , "{- Local Modules Imported -}"
              , "{- End of Module Imports -}"
              ]
    )
  , ( "cabal-file"
    , unlines [ "Name:"
              , "Version:"
              , "License:"
              , "Author:"
              , "Homepage:"
              , "Build-depends:   base"
              , "Synopsis:"
              , ""
              , ""
              , "Executable:"
              , "Main-is:"
              , "Other-modules:"
              , "Include-dirs:"
              , "C-sources:"
              , "Extra-libraries:"
              , "Extensions:"
              , "Ghc-options: -Wall"
              , ""
              ]
    )
  , ( "GPL-2"
    , unlines
      [ "--"
      , "-- This program is free software; you can redistribute it and/or"
      , "-- modify it under the terms of the GNU General Public License as"
      , "-- published by the Free Software Foundation; either version 2 of"
      , "-- the License, or (at your option) any later version."
      , "--"
      , "-- This program is distributed in the hope that it will be useful,"
      , "-- but WITHOUT ANY WARRANTY; without even the implied warranty of"
      , "-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU"
      , "-- General Public License for more details."
      , "--"
      , "-- You should have received a copy of the GNU General Public License"
      , "-- along with this program; if not, write to the Free Software"
      , "-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA"
      , "-- 02111-1307, USA."
      , "--"
      ]
    )
  , ( "cabal-setup"
    , unlines
      [ "import Distribution.Simple"
      , "import System.Cmd"
      , "  ( system )"
      , "import System.Exit"
      , "  ( exitWith )"
      , "import System"
      , "  ( getArgs )"
      , ""
      , "myHooks :: UserHooks"
      , "myHooks = defaultUserHooks"
      , ""
      , "main :: IO ()"
      , "main = do args <- getArgs"
      , "          processArgs args"
      , ""
      , "processArgs :: [ String ] -> IO ()"
      , "processArgs _  = defaultMainWithHooks myHooks"
      ]
    )
  , ( "haskell-script"
    , unlines
      [ "{-"
      , "-}"
      , "module Main"
      , "  ( main )"
      , "where"
      , ""
      , "{- Standard Library Modules Imported -}"
      , "import System.Console.GetOpt"
      , "  ( getOpt"
      , "  , usageInfo"
      , "  , ArgOrder    ( .. )"
      , "  , OptDescr    ( .. )"
      , "  , ArgDescr    ( .. )"
      , "  )"
      , "import System.Environment"
      , "  ( getArgs"
      , "  , getProgName"
      , "  )"
      , "{- External Library Modules Imported -}"
      , "{- Local Modules Imported -}"
      , "{- End of Imports -}"
      , ""
      , "data CliFlag ="
      , "    CliHelp"
      , "  | CliVersion"
      , "  deriving Eq"
      , ""
      , ""
      , "options :: [ OptDescr CliFlag ]"
      , "options ="
      , "  [ Option   \"h\"     [ \"help\" ]"
      , "    (NoArg CliHelp)"
      , "    \"Print the help message to standard out and then exit\""
      , ""
      , "  , Option   \"v\"     [ \"version\" ]"
      , "    (NoArg CliVersion)"
      , "    \"Print out the version of this program\""
      , "  ]"
      , ""
      , "helpMessage :: String -> String"
      , "helpMessage progName ="
      , "  usageInfo progName options"
      , ""
      , "versionMessage :: String -> String"
      , "versionMessage progName = "
      , "  progName ++ \": This is version 0.001\""
      , ""
      , "-- | The main exported function"
      , "main :: IO ()"
      , "main = getArgs >>= processOptions"
      , ""
      , "processOptions :: [ String ] -> IO ()"
      , "processOptions cliArgs ="
      , "  case getOpt Permute  options cliArgs of"
      , "    (flags, args, [])       -> "
      , "      processArgs flags args"
      , "    (_flags, _args, errors) -> "
      , "      do progName <- getProgName"
      , "         ioError $ userError (concat errors ++ helpMessage progName)"
      , ""
      , "-- We assume all of the arguments are files to process"
      , "processArgs :: [ CliFlag ] -> [ String ] -> IO ()"
      , "processArgs flags files"
      , "  | elem CliHelp flags    = getProgName >>= (putStrLn . helpMessage)"
      , "  | elem CliVersion flags = getProgName >>= (putStrLn . versionMessage)"
      , "  | otherwise             = mapM_ processFile files"
      , ""
      , "-- Our processing of a file is to simply count the words"
      , "-- in the file and output the number as a line."
      , "processFile :: FilePath -> IO ()"
      , "processFile file ="
      , "  do contents <- readFile file"
      , "     putStrLn (show $ length $ words contents)"
      , ""
      ]
    )
  ]
