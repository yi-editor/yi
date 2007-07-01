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

        -- * Incremental Search

        isearchInitE,
        isearchIsEmpty,
        isearchAddE,
        isearchNextE,
        isearchWordE,
        isearchDelE,
        isearchCancelE,
        isearchFinishE,

        -- * Replace
        qrNextE,
        qrReplaceOneE

                 ) where

import Yi.Buffer
import Text.Regex.Posix.String  ( Regex, compExtended, compIgnoreCase, compNewline, compile, execBlank )
import Yi.Editor hiding (readEditor)
import qualified Yi.Editor as Editor

import Data.Bits ( (.|.) )
import Data.Char
import Data.Maybe
import Data.List
import Data.Typeable

import Control.Monad.Reader

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
setRegexE re = withEditor $ modifyEditor_ $ \e -> return e { regex = Just re }

-- Return contents of regex register
getRegexE :: YiM (Maybe SearchExp)
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
          -> YiM SearchResult

searchDoE _ GoLeft = do
        errorE "Backward searching is unimplemented"
	return Nothing
searchDoE (s, re) _ = searchF s re

--
-- Set up a search.
--
searchInitE :: String -> [SearchF] -> YiM SearchExp
searchInitE re fs = do
    Right c_re <- lift $ compile (extended .|. igcase .|. newline) execBlank re
    let p = (re,c_re)
    setRegexE p
    return p

    where
        extended | Basic      `elem` fs = 0
                 | otherwise            = compExtended   -- extended regex dflt
        igcase   | IgnoreCase `elem` fs = compIgnoreCase
                 | otherwise            = 0              -- case insensitive dflt
        newline  | NoNewLine  `elem` fs = 0
                 | otherwise            = compNewline    -- newline is special


-- ---------------------------------------------------------------------
-- Internal

--
-- Do a forward search, placing cursor at first char of pattern, if found.
-- Keymaps may implement their own regex language. How do we provide for this?
-- Also, what's happening with ^ not matching sol?
--
searchF :: String -> Regex -> YiM SearchResult
searchF _ c_re = withBuffer $ do
    mp <- do
            p   <- pointB
            rightB               -- start immed. after cursor
            mp  <- regexB c_re
            case fmap Right mp of
                x@(Just _) -> return x
                _ -> do moveTo 0
                        np <- regexB c_re
                        moveTo p
                        return (fmap Left np)
    case mp of
        Just (Right (p,_)) -> moveTo p
        Just (Left  (p,_)) -> moveTo p
	_                  -> return ()
    return mp

------------------------------------------------------------------------
-- Global search and replace
--


------------------------------------------------------------------------
-- | Search and replace /on current line/. Returns Bool indicating
-- success or failure
--
-- TODO too complex.
--
searchAndRepLocal :: String -> String -> YiM Bool
searchAndRepLocal [] _ = return False   -- hmm...
searchAndRepLocal re str = do
    Right c_re <- lift $ compile compExtended execBlank re
    setRegexE (re,c_re)     -- store away for later use

    mp <- withBuffer $ do   -- find the regex
            mp <- regexB c_re
            return mp
    case mp of
        Just (i,j) -> withBuffer $ do
                p  <- pointB      -- all buffer-level atm
                moveToEol
                ep <- pointB      -- eol point of current line
                moveTo i
                moveToEol
                eq <- pointB      -- eol of matched line
                moveTo p          -- go home. sub doesn't move
                if (ep /= eq)       -- then match isn't on current line
                    then return False
                    else do         -- do the replacement
                moveTo i
                deleteN (j - i)
                insertN str
                moveTo p          -- and back to where we were!
                return True -- signal success
        Nothing -> return False


--------------------------
-- Incremental search


newtype Isearch = Isearch [(String, Int)] deriving Typeable

instance Initializable Isearch where
    initial = (Isearch [])

isearchInitE :: YiM ()
isearchInitE = do
  p <- withBuffer pointB
  setDynamic (Isearch [("",p)])
  msgE $ "I-search: "

isearchIsEmpty :: YiM Bool
isearchIsEmpty = do
  Isearch s <- getDynamic
  return $ not $ null $ fst $ head $ s

isearchAddE :: String -> YiM ()
isearchAddE increment = do
  Isearch s <- getDynamic
  let (previous,p0) = head s
  let current = previous ++ increment
  msgE $ "I-search: " ++ current
  prevPoint <- withBuffer pointB
  withBuffer $ moveTo p0
  mp <- withBuffer $ searchB current
  case mp of
    Nothing -> do withBuffer $ moveTo prevPoint -- go back to where we were
                  setDynamic $ Isearch ((current,p0):s)
                  msgE $ "Failing I-search: " ++ current
    Just p -> do setDynamic $ Isearch ((current,p):s)
                 withBuffer $ moveTo (p+length current)

isearchDelE :: YiM ()
isearchDelE = do
  Isearch s <- getDynamic
  case s of
    (_:(text,p):rest) -> do
      withBuffer $ moveTo (p+length text)
      setDynamic $ Isearch ((text,p):rest)
      msgE $ "I-search: " ++ text
    _ -> return () -- if the searched string is empty, don't try to remove chars from it.
    

isearchNextE :: YiM ()
isearchNextE = do
  Isearch ((current,p0):rest) <- getDynamic
  withBuffer $ moveTo (p0 + length current)
  mp <- withBuffer $ searchB current
  case mp of
    Nothing -> return ()
    Just p -> do setDynamic $ Isearch ((current,p):rest)
                 withBuffer $ moveTo (p+length current)

isearchWordE :: YiM ()
isearchWordE = do
  text <- withBuffer (pointB >>= nelemsB 32) -- add maximum 32 chars at a time.
  let (prefix, rest) = span (not . isAlpha) text
      word = takeWhile isAlpha rest
  isearchAddE (prefix ++ word)


isearchFinishE :: YiM ()
isearchFinishE = do
  Isearch s <- getDynamic
  let (_,p0) = last s
  withBuffer $ setSelectionMarkPointB p0
  msgE "mark saved where search started"

isearchCancelE :: YiM ()
isearchCancelE = do
  Isearch s <- getDynamic
  let (_,p0) = last s
  withBuffer $ moveTo p0
  msgE "Quit"
  

-----------------
-- Query-Replace

qrNextE :: FBuffer -> String -> YiM ()
qrNextE b what = do
  mp <- withGivenBuffer b $ searchB what
  case mp of
    Nothing -> do
            msgE "String to search not found"
            closeE
    Just p -> withGivenBuffer b $ do 
                   moveTo p
                   m <- getSelectionMarkB
                   setMarkPointB m (p+length what)
          

qrReplaceOneE :: FBuffer -> String -> String -> YiM ()
qrReplaceOneE b what replacement = do
  lift $ runBuffer b $ do
    deleteN (length what)
    insertN replacement
  qrNextE b what
