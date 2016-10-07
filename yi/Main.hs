{-# LANGUAGE CPP #-}

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import Data.Maybe               (fromMaybe)

import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Buffer.Misc (lineMoveRel)

import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
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
      Nothing -> putStrLn "Yi 0.13.0.1"
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
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
