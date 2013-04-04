module Yi.Keymap.Vim2.Motion
    ( Move(..)
    , CountedMove(..)
    , stringToMove 
    , regionOfMoveB
    , changeMoveStyle
    ) where

-- TODO:
--
-- respecting wrap in gj, g0, etc
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

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_, unless)

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StyledRegion

data Move = Move {
    moveStyle :: !RegionStyle
  , moveIsJump :: !Bool
  , moveAction :: (Maybe Int -> BufferM ())
  }

data CountedMove = CountedMove !(Maybe Int) !Move

-- see :help jump-motions in vim
jumpyMoves :: [String]
jumpyMoves = ["G", "%", "(", ")", "[[", "]]", "{", "}", "L", "M", "H"]

stringToMove :: String -> MatchResult Move
stringToMove s = case lookupMove s of
                    Just m -> WholeMatch (m { moveIsJump = s `elem` jumpyMoves })
                    Nothing -> if any (isPrefixOf s . fst) allNonParametricMotions
                               then PartialMatch
                               else NoMatch
                 <|> matchGotoCharMove s
    
lookupMove :: String -> Maybe Move
lookupMove s = fmap (Move Exclusive False) (lookup s exclusiveMotions)
           <|> fmap (Move Inclusive False) (lookup s inclusiveMotions)
           <|> fmap (Move LineWise  False) (lookup s linewiseMotions)
           <|> fmap (Move LineWise  False) (lookup s linewiseMaybeMotions)

allNonParametricMotions :: [(String, Maybe Int -> BufferM ())]
allNonParametricMotions = concat
    [ exclusiveMotions
    , inclusiveMotions
    , linewiseMotions
    , linewiseMaybeMotions
    ]

changeMoveStyle :: (RegionStyle -> RegionStyle) -> Move -> Move
changeMoveStyle smod (Move s j m) = Move (smod s) j m

-- Linewise motions which treat no count as being the same as a count of 1.
linewiseMotions :: [(String, Maybe Int -> BufferM ())]
linewiseMotions = fmap withDefaultCount
    [ ("j", discard . lineMoveRel)
    , ("k", discard . lineMoveRel . negate)
    , ("-", const firstNonSpaceB <=< discard . lineMoveRel . negate)
    , ("+", const firstNonSpaceB <=< discard . lineMoveRel)
    , ("_", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                firstNonSpaceB)
    , ("gg", discard . gotoLn) -- TODO: save column
    ]

-- Linewise motions which differentiate no count and a count of 1.
linewiseMaybeMotions :: [(String, Maybe Int -> BufferM ())]
linewiseMaybeMotions =
    [ ("G", gotoXOrEOF)
    ]

-- Exclusive motions which treat no count as being the same as a count of 1.
exclusiveMotions :: [(String, Maybe Int -> BufferM ())]
exclusiveMotions = fmap withDefaultCount
    [ ("h", moveXorSol)
    , ("l", moveXorEol)
    , ("w", moveForwardB unitViWord)
    , ("W", moveForwardB unitViWORD)
    , ("b", moveBackwardB unitViWord)
    , ("B", moveBackwardB unitViWORD)
    , ("^", const firstNonSpaceB)
    , ("g^", const firstNonSpaceB) -- TODO: respect wrapping
    , ("g0", const moveToSol) -- TODO: respect wrapping
    -- "0" sort of belongs here, but is currently handled as a special case in some modes
    , ("|", \n -> moveToSol >> moveXorEol (n - 1))
    , ("(", moveBackwardB unitSentence)
    , (")", moveForwardB unitSentence)
    , ("{", moveBackwardB unitEmacsParagraph)
    , ("}", moveForwardB unitEmacsParagraph)
    ]

-- Inclusive motions which treat no count as being the same as a count of 1.
inclusiveMotions :: [(String, Maybe Int -> BufferM ())]
inclusiveMotions = fmap withDefaultCount
    [
    -- Word motions
      ("e", repeat $ genMoveB unitViWord (Forward, InsideBound) Forward)
    , ("E", repeat $ genMoveB unitViWORD (Forward, InsideBound) Forward)
    , ("ge", repeat $ genMoveB unitViWord (Forward, InsideBound) Backward)
    , ("gE", repeat $ genMoveB unitViWORD (Forward, InsideBound) Backward)

    -- Intraline stuff
    , ("g$", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol)
    , ("$", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol
                leftOnEol)
    , ("g_", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                lastNonSpaceB)
    ]
    ++
    [("%", \maybeCount -> case maybeCount of
                Nothing -> findMatchingPairB
                Just percent -> movePercentageFileB percent)
    ]

repeat :: BufferM () -> Int -> BufferM ()
repeat = flip replicateM_

regionOfMoveB :: CountedMove -> BufferM StyledRegion
regionOfMoveB = normalizeRegion <=< regionOfMoveB'

regionOfMoveB' :: CountedMove -> BufferM StyledRegion
regionOfMoveB' (CountedMove n (Move style _isJump move)) = do
    -- region <- mkRegion <$> pointB <*> destinationOfMoveB (move n >> leftOnEol)
    region <- mkRegion <$> pointB <*> destinationOfMoveB
        (move n >> when (style == Inclusive) leftOnEol)
    return $! StyledRegion style region

moveForwardB, moveBackwardB :: TextUnit -> Int -> BufferM ()
moveForwardB unit = repeat $ genMoveB unit (Backward,InsideBound) Forward
moveBackwardB unit = repeat $ moveB unit Backward

gotoXOrEOF :: Maybe Int -> BufferM ()
gotoXOrEOF n = case n of
    Nothing -> botB >> moveToSol
    Just n' -> gotoLn n' >> moveToSol

withDefaultCount :: (String, Int -> BufferM ()) -> (String, Maybe Int -> BufferM ())
withDefaultCount (s, f) = (s, f . defaultCount)
    where defaultCount = fromMaybe 1

matchGotoCharMove :: String -> MatchResult Move
matchGotoCharMove (m:[]) | m `elem` "fFtT" = PartialMatch
matchGotoCharMove (m:c:[]) | m `elem` "fFtT" = WholeMatch $ Move style False action
    where (dir, style, move) =
              case m of
                  'f' -> (Forward, Inclusive, nextCInLineInc c)
                  't' -> (Forward, Inclusive, nextCInLineExc c)
                  'F' -> (Backward, Exclusive, prevCInLineInc c)
                  'T' -> (Backward, Exclusive, prevCInLineExc c)
                  _ -> error "can't happen"
          action mcount = do
                  let count = fromMaybe 1 mcount
                  p0 <- pointB
                  replicateM_ (count - 1) $ do
                      move
                      when (style == Exclusive) $ moveB Character dir
                  p1 <- pointB
                  move
                  p2 <- pointB
                  when (p1 == p2) $ moveTo p0
matchGotoCharMove _ = NoMatch
