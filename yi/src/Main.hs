-- | "Real" Frontend to the static binary.

module Main (main) where

import Control.Monad.State

import Yi.Boot (yiDriver')
import Yi.Config.Default (defaultConfig)

import Yi.Config.Default.Vty
import Yi.Config.Default.Emacs
import Yi.Config.Default.HaskellMode
import Yi.Config.Default.JavaScriptMode
import Yi.Config.Default.MiscModes
import Yi.Config.Simple.Types

main :: IO ()
main = do
    cfg <- execStateT configure defaultConfig
    yiDriver' True cfg
    where
    configure = runConfigM $ do
        configureVty
        configureEmacs
        configureHaskellMode
        configureJavaScriptMode
        configureMiscModes