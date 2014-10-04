{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Search.Internal
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal use for Yi.Search.

module Yi.Search.Internal where

import Control.Lens (assign, use)
import Yi.Editor (EditorM, currentRegexA)
import Yi.Regex (SearchExp)

-- ---------------------------------------------------------------------
-- Searching and substitutions with regular expressions
--
-- The most recent regex is held by the editor. You can get at it with
-- getRegeE. This is useful to determine if there was a previous
-- pattern.
--

-- | Put regex into regex 'register'
setRegexE :: SearchExp -> EditorM ()
setRegexE re = assign currentRegexA (Just re)

-- | Clear the regex 'register'
resetRegexE :: EditorM ()
resetRegexE = assign currentRegexA Nothing

-- | Return contents of regex register
getRegexE :: EditorM (Maybe SearchExp)
getRegexE = use currentRegexA
