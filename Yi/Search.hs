--
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007 Jean-Philippe Bernardy
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

-- | Search/Replace functions
--

module Yi.Search (
        setRegexE,      -- :: SearchExp -> Action
        getRegexE,      -- :: IO (Maybe SearchExp)
        SearchMatch,
        SearchExp,
        SearchF(..),
        searchAndRepLocal,  -- :: String -> String -> IO Bool
        searchE,            -- :: (Maybe String) -> [SearchF]
                            -- -> Direction -> Action
        searchInitE,        -- :: String
                            -- -> [SearchF]
                            -- -> IO SearchExp
        searchDoE,          -- :: SearchExp
                            -- -> Direction
                            -- -> IO SearchResult
                 ) where



import Yi.Debug
import Yi.Buffer
import Text.Regex.Posix.Wrap    ( Regex,  compExtended, compIgnoreCase, compNewline, wrapCompile, execBlank)
import Yi.Editor
import qualified Yi.Editor as Editor

import Data.Bits ( (.|.) )
import Data.Maybe
import Data.List

import Control.Monad

import Foreign.C.String

import Yi.Core

-- ---------------------------------------------------------------------
-- Searching and substitutions with regular expressions
--
-- The most recent regex is held by the editor. You can get at it with
-- getRegeE. This is useful to determine if there was a previous
-- pattern.
--

-- | Put regex into regex 'register'
setRegexE :: SearchExp -> Action
setRegexE re = modifyEditor_ $ \e -> return e { regex = Just re }

-- Return contents of regex register
getRegexE :: IO (Maybe SearchExp)
getRegexE = readEditor regex


-- ---------------------------------------------------------------------
--
-- | Global searching. Search for regex and move point to that position.
-- @Nothing@ means reuse the last regular expression. @Just s@ means use
-- @s@ as the new regular expression. Direction of search can be
-- specified as either @Left@ (backwards) or @Right@ (forwards in the
-- buffer). Arguments to modify the compiled regular expression can be
-- supplied as well.
--

--
-- What would be interesting would be to implement our own general
-- mechanism to allow users to supply a regex function of any kind, and
-- search with that. This removes the restriction on strings be valid
-- under regex(3).
--

data SearchF = Basic        -- ^ Use non-modern (i.e. basic) regexes
             | IgnoreCase   -- ^ Compile for matching that ignores char case
             | NoNewLine    -- ^ Compile for newline-insensitive matching
    deriving Eq

type SearchMatch = (Int, Int)
type SearchResult = Maybe (Either SearchMatch SearchMatch)
type SearchExp = (String, Regex)

searchE :: (Maybe String)       -- ^ @Nothing@ means used previous
                                -- pattern, if any. Complain otherwise.
                                -- Use getRegexE to check for previous patterns
        -> [SearchF]            -- ^ Flags to modify the compiled regex
        -> Direction            -- ^ @Left@ means backwards, @Right@ means forward
        -> Action

searchE s fs d =
     case s of
        Just re -> searchInitE re fs >>= (flip searchDoE) d >>= f
        Nothing -> do
	    mre <- getRegexE
            case mre of
                Nothing -> errorE "No previous search pattern" -- NB
                Just r -> searchDoE r d >>= f
    where
        f mp = case mp of
            Just (Right _) -> return ()
            Just (Left  _) -> msgE "Search wrapped"
            Nothing        -> errorE "Pattern not found"


searchDoE :: SearchExp
          -> Direction
          -> IO SearchResult

searchDoE _ GoLeft = do
        errorE "Backward searching is unimplemented"
	return Nothing
searchDoE (s, re) _ = searchF s re

--
-- Set up a search.
--
searchInitE :: String -> [SearchF] -> IO SearchExp
searchInitE re fs = do
    Right c_re <- withCString re $ \re' -> wrapCompile (extended .|. igcase .|. newline) execBlank re'
    let p = (re,c_re)
    setRegexE p
    return p

    where
        extended | Basic      `elem` fs = 0
                 | otherwise            = compExtended   -- extended regex dflt
        igcase   | IgnoreCase `elem` fs = compIgnoreCase
                 | otherwise            = 0             -- case insensitive dflt
        newline  | NoNewLine  `elem` fs = 0
                 | otherwise            = compNewline    -- newline is special


-- ---------------------------------------------------------------------
-- Internal

--
-- Do a forward search, placing cursor at first char of pattern, if found.
-- Keymaps may implement their own regex language. How do we provide for this?
-- Also, what's happening with ^ not matching sol?
--
searchF :: String -> Regex -> IO SearchResult
searchF _ c_re = do
    mp <- withBuffer $ \b -> do
            p   <- pointB b
            rightB b                  -- start immed. after cursor
            mp  <- regexB b c_re
            case fmap Right mp of
                x@(Just _) -> return x
                _ -> do moveTo b 0
                        np <- regexB b c_re
                        moveTo b p
                        return (fmap Left np)
    case mp of
        Just (Right (p,_)) -> gotoPointE p >> return mp
        Just (Left  (p,_)) -> gotoPointE p >> return mp
	_                  -> return mp

------------------------------------------------------------------------
-- Global search and replace
--


------------------------------------------------------------------------
-- | Search and replace /on current line/. Returns Bool indicating
-- success or failure
--
-- TODO too complex.
--
searchAndRepLocal :: String -> String -> IO Bool
searchAndRepLocal [] _ = return False   -- hmm...
searchAndRepLocal re str = do
    Right c_re <- withCString re $ \re' -> wrapCompile compExtended execBlank re'
    setRegexE (re,c_re)     -- store away for later use

    mp <- withBuffer $ \b -> do   -- find the regex
            mp <- regexB b c_re
            return mp
    case mp of
        Just (i,j) -> withBuffer $ \b -> do
                p  <- pointB b      -- all buffer-level atm
                moveToEol b
                ep <- pointB b      -- eol point of current line
                moveTo b i
                moveToEol b
                eq <- pointB b      -- eol of matched line
                moveTo b p          -- go home. sub doesn't move
                if (ep /= eq)       -- then match isn't on current line
                    then return False
                    else do         -- do the replacement
                moveTo b i
                deleteN b (j - i)
                insertN b str
                moveTo b p          -- and back to where we were!
                return True -- signal success
        Nothing -> return False
