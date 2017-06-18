{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Data.Prototype
import           Lens.Micro.Platform              ((.=))
import           Yi.Boot                          (configMain, reload)
import           Yi.Config
import           Yi.Config.Default                (defaultConfig)
import           Yi.Config.Default.HaskellMode    (configureHaskellMode)
import           Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import           Yi.Config.Default.MiscModes      (configureMiscModes)
import           Yi.Config.Default.Vim
import           Yi.Config.Default.Vty
import           Yi.Config.Simple hiding          (super)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Rope as R

main :: IO ()
main = configMain defaultConfig $ do
         configureVty
         myVimConfig
         configureHaskellMode
         configureJavaScriptMode
         configureMiscModes

myVimConfig :: ConfigM ()
myVimConfig = do
  configureVim
  defaultKmA .= myKeymapSet
  modeTableA .= myModes
  configCheckExternalChangesObsessivelyA .= False

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super {
          -- Here we can add custom bindings.
          -- See Yi.Keymap.Vim.Common for datatypes and
          -- Yi.Keymap.Vim.Utils for useful functions like mkStringBindingE

          -- In case of conflict, that is if there exist multiple bindings
          -- whose prereq function returns WholeMatch,
          -- the first such binding is used.
          -- So it's important to have custom bindings first.
          V.vimBindings = myBindings eval <> V.vimBindings super
        , V.vimExCommandParsers = exReload : V.vimExCommandParsers super
        }

myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
    let nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
        imap x y = V.VimBindingE (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Continue))
                                             (evs `V.matchesString` x)
                                    _ -> V.NoMatch)
    in [ nmap "<C-h>" previousTabE
       , nmap "<C-l>" nextTabE

         -- Press space to clear incremental search highlight
       , nmap " " (eval ":nohlsearch<CR>")

       , nmap "<F3>" (withCurrentBuffer deleteTrailingSpaceB)
       , nmap "<F4>" (withCurrentBuffer moveToSol)
       , nmap "<F1>" (withCurrentBuffer readCurrentWordB >>= printMsg . R.toText)
       , imap "<Home>" (withCurrentBuffer moveToSol)
       , imap "<End>" (withCurrentBuffer moveToEol)
       ]

myModes :: [AnyMode]
myModes = [
         AnyMode Haskell.fastMode {
             -- Disable beautification
             modePrettify = const $ return ()
         }
    ]

exReload :: V.EventString -> Maybe V.ExCommand
exReload "reload" = Just $ V.impureExCommand {
    V.cmdShow = "reload"
  , V.cmdAction = YiA reload
  }
exReload _ = Nothing
