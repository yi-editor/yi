-- | Information about the current system used to host Yi. This includes the libraries (with exact
-- version) available and what main to use.
module Yi.System.Info ( module Yi.System.Info
                      , module Distribution.Package
                      , module Distribution.PackageDescription
                      , module Distribution.Simple.LocalBuildInfo
                      ) where

import Yi.System.Info.Data

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

data SystemInfo = SystemInfo
    { yiPackageDescription :: PackageDescription
    , yiLocalBuildInfo :: LocalBuildInfo
    } deriving (Show, Read)

yiSystemInfo = SystemInfo
    { yiPackageDescription = read rawPackageDescriptionStr
    , yiLocalBuildInfo = read rawLocalBuildInfoStr
    }

