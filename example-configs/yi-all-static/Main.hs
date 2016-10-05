{-# LANGUAGE CPP #-}
-- This example requires manual rebuild (as opposed to dynamic ones, automatically rebuilding the
-- config upon changes). This config is useful for distribution of the editor in binary form as such
-- a build have almost all libraries statically linked in.
-- Here's a building example with "stack":
-- 1. Edit "stack.yaml" file so that "location: " would point to the root of the Yi source code
-- 2. Run "stack install"
-- 3. Run the built with "stack exec yi" (or just "yi" if you have $PATH
--    configured acc.)
-- The final name of the executable can be changed in the "package.yaml" file.

import Control.Monad.State.Lazy
import Data.List
import Lens.Micro.Platform
import System.Environment
import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)

#ifdef VIM
import Yi.Config.Default.Vim (configureVim)
#endif
#ifdef VTY
import Yi.Config.Default.Vty (configureVty)
#endif
#ifdef EMACS
import Yi.Config.Default.Emacs (configureEmacs)
#endif
#ifdef PANGO
import Yi.Config.Default.Pango (configurePango)
#endif

data CommandLineOptions = CommandLineOptions {
    frontend :: Maybe String
  , keymap :: Maybe String
  , startOnLine :: Maybe Integer
  , files :: [String]
  }

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = flag' Nothing 
                       ( long "version" 
                      <> short 'v' 
                      <> help "Show the version number") 
  <|> (Just <$> (CommandLineOptions
    <$> optional (strOption
        ( long "frontend"
       <> short 'f'
       <> metavar "FRONTEND"
       <> help "The frontend to use"))
    <*> optional (strOption
        ( long "keymap"
       <> short 'k'
       <> metavar "KEYMAP"
       <> help "The keymap to use"))
    <*> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the file on line NUM (not yet working)"))
    <*> many (argument str (metavar "FILES..."))
  ))

main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of 
      Nothing -> putStrLn "Yi 0.13.0.1"
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
        cfg <- execStateT
            (runConfigM (myConfig (frontend clo) (keymap clo) >> (startActionsA .= openFileActions)))
            defaultConfig
        startEditor cfg Nothing
  where 
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "yi - a flexible extensible text editor written in haskell")

myConfig :: Maybe String -> Maybe String -> ConfigM ()
myConfig f k = do
#ifdef PANGO
  if f == Just "pango" || f == Nothing
    then configurePango
    else return ()
#endif
#ifdef VTY
#ifndef PANGO
  if f == Just "vty" || f == Nothing
#else
  if f == Just "vty"
#endif
    then configureVty
    else return ()
#endif
#ifdef EMACS
  if k == Just "emacs" || k == Nothing
    then configureEmacs
    else return ()
#endif
#ifdef VIM
#ifndef EMACS
  if k == Just "vim" || k == Nothing
#else
  if k == Just "vim"
#endif
    then configureVim
    else return ()
#endif
  configureHaskellMode
  configureMiscModes
