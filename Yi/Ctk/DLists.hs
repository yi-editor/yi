--  The Compiler Toolkit: difference lists
--
--  Author : Manuel M. T. Chakravarty
--  Created: 24 February 95
--
--  Copyright (c) [1995..2000] Manuel M. T. Chakravarty
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module provides the functional equivalent of the difference lists
--  from logic programming.  They provide an O(1) append.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module Yi.Ctk.DLists (
        DList, openDL, zeroDL, unitDL, snocDL, joinDL, closeDL
    ) where

-- a difference list is a function that given a list returns the original
-- contents of the difference list prepended at the given list (EXPORTED)
--
type DList a = [a] -> [a]

-- open a list for use as a difference list (EXPORTED)
--
openDL :: [a] -> DList a
openDL  = (++)

-- create a difference list containing no elements (EXPORTED)
--
zeroDL :: DList a
zeroDL  = id

-- create difference list with given single element (EXPORTED)
--
unitDL :: a -> DList a
unitDL  = (:)

-- append a single element at a difference list (EXPORTED)
--
snocDL      :: DList a -> a -> DList a
snocDL dl x  = \l -> dl (x:l)

-- appending difference lists (EXPORTED)
--
joinDL :: DList a -> DList a -> DList a
joinDL  = (.)

-- closing a difference list into a normal list (EXPORTED)
--
closeDL :: DList a -> [a]
closeDL  = ($[])
