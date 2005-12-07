--
-- Copyright (c) 2005 Stefan Wehr (http://www.stefanwehr.de)
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- Test the testsuite
--
module Main where

import Process
import TestFramework

$(tests "myTests1" [d|

 test1 = assertEqual 1 1

 test2 = assertEqual2 2 2

 test3 = assertNull []

 test4 = assertNotNull "Stefan"

 test5 = assert True

 |])


$(tests "myTests2" [d| |])

$(tests "myTests3" [d|

 test1 = assertEqual 2 3

 |])

allTests = TestList [myTests1, myTests2, myTests3]
main = runTestTT allTests
