{-# LANGUAGE CPP #-}
-- This example requires manual rebuild (as opposed to dynamic ones, automatically rebuilding the
-- config upon changes). This config is useful for distribution of the editor in binary form as such
-- a build have almost all libraries statically linked in.
-- Here's a building example with "stack":
-- 1. Edit "stack.yaml" file so that "location: " would point to the root of the Yi source code
-- 2. Run "stack install"
--  (You can add "--flag yi-all-static:-vty" for example to leave out the vty
--  frontend, see the package.yaml file for all available flags)
-- 3. Run the built with "stack exec yi" (or just "yi" if you have $PATH
--    configured acc.)
--    (Please look at the command line options with "stack exec yi -- -h")
-- The final name of the executable can be changed in the "package.yaml" file.

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))

import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types

import Yi.Buffer.Misc (lineMoveRel)
import Yi.Config.Default.MiscModes      (configureMiscModes)

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

frontends :: [(String, ConfigM ())]
frontends = [
#ifdef PANGO
  ("pango", configurePango),
#endif
#ifdef VTY
  ("vty", configureVty),
#endif
  ("", return ())
  ]

keymaps :: [(String, ConfigM ())]
keymaps = [
#ifdef EMACS
  ("emacs", configureEmacs),
#endif
#ifdef VIM
  ("vim", configureVim),
#endif
  ("", return ())
  ]

data CommandLineOptions = CommandLineOptions {
    frontend :: Maybe String
  , keymap :: Maybe String
  , startOnLine :: Maybe Int
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
       <> help "The frontend to use (default is pango)"))
    <*> optional (strOption
        ( long "keymap"
       <> short 'k'
       <> metavar "KEYMAP"
       <> help "The keymap to use (default is emacs)"))
    <*> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the (last) file on line NUM"))
    <*> many (argument str (metavar "FILES..."))
  ))

main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.15.0"
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        cfg <- execStateT
            (runConfigM (myConfig (frontend clo) (keymap clo) >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
            defaultConfig
        startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in haskell")

myConfig :: Maybe String -> Maybe String -> ConfigM ()
myConfig f k = do
  -- Lookup f in the frontends list or pick the first element of the frontends list if
  -- f is nothing or do nothing if f is not found in the frontends list.
  case f of
    Nothing -> snd (head frontends)
    Just f' -> fromMaybe (return ()) (lookup f' frontends)
  -- Same as above, but then with k and keymaps
  case k of
    Nothing -> snd (head keymaps)
    Just k' -> fromMaybe (return ()) (lookup k' keymaps)
  configureMiscModes
