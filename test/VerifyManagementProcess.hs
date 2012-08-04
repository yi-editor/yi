{-# LANGUAGE BangPatterns #-}
module VerifyManagementProcess where

import Prelude hiding ( concat )

import Yi.System.Info

import Yi.Test.BatchInteract
import Yi.Test.Prelude

tests :: IO [Test]
tests = return $ concat [ version_opts_tests, info_opts_tests ]

version_opts_tests :: [Test]
version_opts_tests = foldMap tests_for_version_opt ["-v", "--version"] 
    where 
        tests_for_version_opt v_opt = 
            [ verify_stdout v_opt [v_opt] $ \out -> do
                let expected = "^version: .*$"
                assert (out =~ expected) "output did not include version" 
            ] 

info_opts_tests :: [Test]
info_opts_tests = 
    [ verify_stdout "raw info is parsable" ["--raw-info"] $ \out -> do
        let parsed_info :: SystemInfo = read out
        assert (show parsed_info == out) "raw info read/show isomorphism"
    , verify_stdout "info is usable" ["--info"] $ \out -> do
        assert (length out > 4) "output is not too tiny"
    ]

