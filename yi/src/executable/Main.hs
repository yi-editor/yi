-- | "Real" Frontend to the static binary.

module Main (main) where

import Yi.Boot (yiDriver)
import Yi.Config.Default (defaultConfig)

main :: IO ()
main = yiDriver defaultConfig
