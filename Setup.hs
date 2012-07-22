#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

import System.Directory
import System.FilePath

import Text.Printf

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
        buildHook = \ pd lbi uh bf -> do
            -- Generate the Yi.System.Info.Data file
            let data_file_dir = buildDir lbi </> "autogen/Yi/System/Info"
            createDirectoryIfMissing True data_file_dir
            let data_file_path = data_file_dir </> "Data.hs"
                definitions = [ ("rawPackageDescriptionStr", show pd)
                              , ("rawLocalBuildInfoStr", show lbi)
                              ]
                -- the values are dumped as numeric escapes to avoid issues with properly
                -- escaping/unescaping
                value_text = concatMap (\c -> "\\" ++ show (fromEnum c))
                definitions_text = concatMap (\(i, v) -> i ++ " = \"" ++ value_text v ++ "\"\n")
            writeFile data_file_path $ 
                "module Yi.System.Info.Data where\n"
                ++ (definitions_text definitions)

            -- Continue with the normal build system
            buildHook simpleUserHooks pd lbi uh bf
    }

