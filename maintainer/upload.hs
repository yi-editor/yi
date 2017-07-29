#!/usr/bin/env runghc

import Control.Monad (forM_, when)
import Data.List (isPrefixOf)
import System.Directory (getDirectoryContents)
import System.Process (callProcess)

main :: IO ()
main = do
    packages <- filter (isPrefixOf "yi") <$> getDirectoryContents "."

    forM_ packages $ \p -> do
        putStrLn $ "\nPreparing " ++ p
        callProcess "stack" ["sdist", p]

    forM_ packages $ \p ->
        when (p /= "yi-intero") $
            callProcess "stack" ["upload", "--no-signature", p]