module Yi.Keymap.Vim.Search
    ( doVimSearch
    , continueVimSearch
    ) where

import Data.Maybe (listToMaybe)

import Yi.Buffer.Adjusted
import Yi.Editor
import Yi.Search

doVimSearch :: Maybe String -> [SearchOption] -> Direction -> EditorM ()
doVimSearch Nothing _ dir = do
    mbRegex <- getRegexE
    case mbRegex of
        Just regex -> withBuffer0 $ continueVimSearch (regex, dir)
        Nothing -> printMsg "No previous search pattern"
doVimSearch (Just needle) opts dir =
    searchInit needle dir opts >>= withBuffer0 . continueVimSearch

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
