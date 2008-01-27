--
--
--

-- | Templates for inserting into documents
--

module Yi.Templates
  ( templates
  , templateNames
  , lookupTemplate
  , addTemplate
  )
where

{- Standard Library Modules Imported -}
import qualified Data.Map as Map
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Yi.Buffer
  ( BufferM
  , insertN
  )
import Yi.Editor
  ( EditorM
  , withBuffer0
  , printMsg
  )
{- End of Module Imports -}

type Template       = String
type TemplateName   = String
type TemplateLookup = Map.Map TemplateName Template

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
  ]

addTemplate :: String -> EditorM ()
addTemplate tName =
  case lookupTemplate tName of
    Nothing -> printMsg "template-name not found"
    Just t  -> withBuffer0 $ addTemplateBuffer t
  where
  addTemplateBuffer :: Template -> BufferM ()
  addTemplateBuffer t = insertN t
