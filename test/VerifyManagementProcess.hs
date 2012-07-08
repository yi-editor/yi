module VerifyManagementProcess where

import Prelude hiding ( concat )

import Yi.Test.BatchInteract
import Yi.Test.Prelude

tests :: IO [Test]
tests = return $ concat [ verify_v_opts ]

verify_v_opts :: [Test]
verify_v_opts = foldMap tests_for_version_opt ["-v", "--version"] 
    where 
        tests_for_version_opt v_opt = 
            [ verify_stdout v_opt [v_opt] $ \out -> do
                let expected = "^version: .*$"
                assert (out =~ expected) "output did not include version" 
            ] 

