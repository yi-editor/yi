{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Search
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Search (doVimSearch, continueVimSearch) where

import Data.Maybe         (listToMaybe)
import Data.Text          ()
import Yi.Buffer.Adjusted
import Yi.Editor          (EditorM, printMsg, withCurrentBuffer)
import Yi.Search          (SearchOption, getRegexE, searchInit)

doVimSearch :: Maybe String -> [SearchOption] -> Direction -> EditorM ()
doVimSearch Nothing _ dir = do
    mbRegex <- getRegexE
    case mbRegex of
        Just regex -> withCurrentBuffer $ continueVimSearch (regex, dir)
        Nothing -> printMsg "No previous search pattern"
doVimSearch (Just needle) opts dir =
    searchInit needle dir opts >>= withCurrentBuffer . continueVimSearch

continueVimSearch :: (SearchExp, Direction) -> BufferM ()
continueVimSearch (searchExp, dir) = do
    mp <- savingPointB $ do
        moveB Character dir  -- start immed. after cursor
        rs <- regexB dir searchExp
        moveB Document (reverseDir dir) -- wrap around
        ls <- regexB dir searchExp
        return $ listToMaybe $ rs ++ ls
    -- regionFirst doesn't work right here, because something inside
    -- Buffer.Implementation.regexRegionBI breaks Region invariant and
    -- may return Region (Forward, A, B) where A > B
    -- TODO: investigate
    maybe (return ()) (moveTo . regionFirst') mp

regionFirst' :: Region -> Point
regionFirst' r = Point $ min a b
    where a = fromPoint $ regionStart r
          b = fromPoint $ regionEnd r
