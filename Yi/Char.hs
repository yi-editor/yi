-- 
-- Copyright (c) 2005 Tuomo Valkonen
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
-- | Same character classification and remapping routines.
--

module Yi.Char (
    upcaseCtrl, 
    lowcaseCtrl,
    upcaseLowcase, 
    ctrlLowcase,
    lowcaseUpcase, 
    ctrlUpcase,
    validChar,
    remapChar,
    remapBS,
    isDel,
    isEnter
) where

import Data.Char
import Yi.Yi ( keyBackspace )

validChar :: Char -> Bool
validChar '\n' = True
validChar '\r' = True
validChar c | isControl c = False
validChar _    = True

-- Remap a sequence of keys to another sequence.
remapChar :: Char -> Char -> Char -> Char -> Char -> Char
remapChar a1 b1 a2 _ c
    | a1 <= c && c <= b1 = chr $ ord c - ord a1 + ord a2
    | otherwise          = c

upcaseCtrl, lowcaseCtrl :: Char -> Char
upcaseLowcase, ctrlLowcase :: Char -> Char
lowcaseUpcase, ctrlUpcase :: Char -> Char
upcaseCtrl    = remapChar '\^A' '\^Z' 'A'   'Z'
lowcaseCtrl   = remapChar '\^A' '\^Z' 'a'   'z'
upcaseLowcase = remapChar 'a'   'z'   'A'   'Z'
ctrlLowcase   = remapChar 'a'   'z'   '\^A' '\^Z'
lowcaseUpcase = remapChar 'A'   'Z'   'a'   'z'
ctrlUpcase    = remapChar 'A'   'Z'   '\^A' '\^Z'

remapBS :: Char -> Char
remapBS k | isDel k = '\BS'
          | otherwise = k

isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False

isEnter :: Char -> Bool
isEnter '\n' = True
isEnter '\r' = True
isEnter _    = False
