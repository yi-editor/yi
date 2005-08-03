{-# OPTIONS -cpp -fglasgow-exts #-}
-- 
-- Copyright (c) 2005 Jean-Philippe Bernardy
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

module Yi.Keymap.Emacs.Keys (readKey, showKey, printableChars) where

import Yi.Char
import Yi.Yi hiding (string)
import qualified Yi.Map as M

import Data.Char
import Data.List
import Data.Maybe

import Text.ParserCombinators.ReadP 

#if __GLASGOW_HASKELL__ < 604
--
-- just enough parsec for 6.2.2 to build
--

import Control.Monad

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

many :: ReadP a -> ReadP [a]
many p = return [] +++ many1 p

many1 :: ReadP a -> ReadP [a]
many1 p = liftM2 (:) p (many p)

#endif


printableChars :: [Char]
printableChars = map chr [32..127]


-- * Key parser 

c_ :: Char -> Char
c_ ' ' = '\0'
c_ x = ctrlLowcase x

m_ :: Char -> Char
m_ '\263' = chr 255
m_ x = setMeta x

parseCtrl :: ReadP Char
parseCtrl = do string "C-"
               k <- parseRegular
               return $ c_ k

parseMeta :: ReadP Char
parseMeta = do string "M-"
               k <- parseRegular
               return $ m_ k

parseCtrlMeta :: ReadP Char
parseCtrlMeta = do string "C-M-"
                   k <- parseRegular
                   return $ m_ $ c_ k
                   

keyNames :: [(Char, String)]
keyNames = [(' ', "SPC"),
            (keyLeft, "<left>"),
            (keyRight, "<right>"),
            (keyDown, "<down>"),
            (keyUp, "<up>"),
            (keyBackspace, "DEL"),
            (keyNPage, "<next>"),
            (keyPPage, "<prior>")
           ]

parseRegular :: ReadP Char
parseRegular = choice [string s >> return c | (c,s) <- keyNames]
               +++ satisfy (`elem` printableChars)               

parseKey :: ReadP String
parseKey = sepBy1 (choice [parseCtrlMeta, parseCtrl, parseMeta, parseRegular])
                  (munch1 isSpace)

readKey :: String -> String
readKey s = case readKey' s of
              [r] -> r
              rs -> error $ "readKey: " ++ s ++ show (map ord s) ++ " -> " ++ show rs

readKey' :: String -> [String]
readKey' s = map fst $ nub $ filter (null . snd) $ readP_to_S parseKey $ s

-- * Key printer
-- FIXME: C- and M- should be swapped when they are both there.
showKey :: String -> String
showKey = dropSpace . printable'
    where 
        printable' ('\ESC':a:ta) = "M-" ++ [a] ++ printable' ta
        printable' ('\ESC':ta) = "ESC " ++ printable' ta
        printable' (a:ta) 
                | ord a < 32
                = "C-" ++ [chr (ord a + 96)] ++ " " ++ printable' ta
                | isMeta a
                = "M-" ++ printable' (clrMeta a:ta)
                | ord a >= 127
                = bigChar a ++ " " ++ printable' ta 
                | otherwise  = [a, ' '] ++ printable' ta
                
        printable' [] = []

        bigChar c = fromMaybe [c] $ M.lookup c $ M.fromList keyNames
