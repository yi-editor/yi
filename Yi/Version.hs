--
-- riot/Version.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- | Version information
--

module Yi.Version where

package, branch, release, version :: String

package = "yi"
branch  = "0"
release = "yyyymmdd"
version = branch++"-"++release

