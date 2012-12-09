module Yi.Keymap.Vim2.Motion
    ( Move(..)
    , CountedMove(..)
    , stringToMove 
    , regionOfMoveB
    , changeMoveStyle
    ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)
import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Keymap.Vim2.StyledRegion

data Move = Move !RegionStyle (Maybe Int -> BufferM ())

data CountedMove = CountedMove !Int !Move


-- TODO:
--
-- respecting wrap in gj, g0, etc
--
-- f, F, t, T, ';' and ',' are currently implemented in a separate mode,
-- preventing their use as operands
--
-- gm, %, go
-- ]], [[, [], ][
-- [(, [{, ]), ]}
-- ]m, ]M, [m, [M
-- [#, ]#
-- [*, [/, ]*, ]/
-- H, M, L
--
-- Moving to marks
--
-- jump-motions are not specified
--
-- Traversing jumplist and changelist

-- TODO:
-- from vim help:
--
-- Special case: "cw" and "cW" are treated like "ce" and "cE" if the cursor is
-- on a non-blank.  This is because "cw" is interpreted as change-word, and a
-- word does not include the following white space.  {Vi: "cw" when on a blank
-- followed by other blanks changes only the first blank; this is probably a
-- bug, because "dw" deletes all the blanks}
--
-- Another special case: When using the "w" motion in combination with an
-- operator and the last word moved over is at the end of a line, the end of
-- that word becomes the end of the operated text, not the first word in the
-- next line.
--
-- The original Vi implementation of "e" is buggy.  For example, the "e" command
-- will stop on the first character of a line if the previous line was empty.
-- But when you use "2e" this does not happen.  In Vim "ee" and "2e" are the
-- same, which is more logical.  However, this causes a small incompatibility
-- between Vi and Vim.

stringToMove :: String -> Maybe Move
stringToMove c = fmap (Move Exclusive) (lookup c exclusiveMotions)
             <|> fmap (Move Inclusive) (lookup c inclusiveMotions)
             <|> fmap (Move LineWise) (lookup c linewiseMotions)

changeMoveStyle :: (RegionStyle -> RegionStyle) -> Move -> Move
changeMoveStyle smod (Move s m) = Move (smod s) m

linewiseMotions :: [(String, Maybe Int -> BufferM ())]
linewiseMotions =
    [ ("j", discard . lineMoveRel . defaultCount)
    , ("k", discard . lineMoveRel . negate . defaultCount)
    , ("-", const firstNonSpaceB <=< discard . lineMoveRel . negate . defaultCount)
    , ("+", const firstNonSpaceB <=< discard . lineMoveRel . defaultCount)
    , ("_", \n' -> do
                let n = defaultCount n'
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                firstNonSpaceB)
    , ("G", gotoXOrEOF)
    , ("gg", discard . gotoLn . defaultCount) -- TODO: save column
    ]

exclusiveMotions :: [(String, Maybe Int -> BufferM ())]
exclusiveMotions =
    [ ("h", moveXorSol . defaultCount)
    , ("l", moveXorEol . defaultCount)
    , ("w", moveForwardB unitViWord)
    , ("W", moveForwardB unitViWORD)
    , ("b", moveBackwardB unitViWord)
    , ("B", moveBackwardB unitViWORD)
    , ("^", const firstNonSpaceB . defaultCount)
    , ("g^", const firstNonSpaceB . defaultCount) -- TODO: respect wrapping
    , ("g0", const moveToSol . defaultCount) -- TODO: respect wrapping
    -- "0" sort of belongs here, but is currently handled as a special case in some modes
    , ("|", (\n -> moveToSol >> moveXorEol (n - 1)) . defaultCount)
    , ("(", moveBackwardB unitSentence)
    , (")", moveForwardB unitSentence)
    , ("{", moveBackwardB unitEmacsParagraph)
    , ("}", moveForwardB unitEmacsParagraph)
    ]

inclusiveMotions :: [(String, Maybe Int -> BufferM ())]
inclusiveMotions =
    [
    -- Word motions
      ("e", repeat $ genMoveB unitViWord (Forward, InsideBound) Forward)
    , ("E", repeat $ genMoveB unitViWORD (Forward, InsideBound) Forward)
    , ("ge", repeat $ genMoveB unitViWord (Forward, InsideBound) Backward)
    , ("gE", repeat $ genMoveB unitViWORD (Forward, InsideBound) Backward)

    -- Intraline stuff
    , ("g$", \n' -> do
                let n = defaultCount n'
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol)
    , ("$", \n' -> do
                let n = defaultCount n'
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol
                leftOnEol)
    , ("g_", \n' -> do
                let n = defaultCount n'
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                lastNonSpaceB)
    ]

repeat :: BufferM () -> Maybe Int -> BufferM ()
repeat b n = replicateM_ (defaultCount n) b

regionOfMoveB :: CountedMove -> BufferM StyledRegion
regionOfMoveB = normalizeRegion <=< regionOfMoveB'

regionOfMoveB' :: CountedMove -> BufferM StyledRegion
regionOfMoveB' (CountedMove n (Move style move)) = do
    -- region <- mkRegion <$> pointB <*> destinationOfMoveB (move n >> leftOnEol)
    region <- mkRegion <$> pointB <*> destinationOfMoveB
        (move (Just n) >> when (style == Inclusive) leftOnEol)
    return $! StyledRegion style region

moveForwardB, moveBackwardB :: TextUnit -> Maybe Int -> BufferM ()
moveForwardB unit = repeat $ genMoveB unit (Backward,InsideBound) Forward
moveBackwardB unit = repeat $ moveB unit Backward

gotoXOrEOF :: Maybe Int -> BufferM ()
gotoXOrEOF n = do
    case n of
        Nothing -> botB >> moveToSol
        Just n' -> gotoLn n' >> moveToSol


defaultCount :: Maybe Int -> Int
defaultCount = fromMaybe 1
