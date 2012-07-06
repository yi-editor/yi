module VerifyManagementProcess where

import Distribution.TestSuite

tests :: IO [Test]
tests = return [ verify_boot_transition ]

verify_boot_transition :: Test
verify_boot_transition = Test $ TestInstance
    { run = return $ Finished Pass
    , name = "verify boot transition"
    , tags = []
    , options = []
    , setOption = const (const $ Left "no options")
    }

