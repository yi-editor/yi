module Yi.Keymap.Vim2.Motion
    ( Move(..)
    , CountedMove(..)
    , stringToMove 
    , regionOfMoveB
    ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Yi.Buffer
import Yi.Keymap.Vim2.StyledRegion

data Move = Move !RegionStyle (Int -> BufferM ())

data CountedMove = CountedMove !Int !Move

stringToMove :: String -> Maybe Move
stringToMove c = fmap (Move Exclusive) (lookup c exclusiveMotions)
             <|> fmap (Move Inclusive) (lookup c inclusiveMotions)

exclusiveMotions :: [(String, Int -> BufferM ())]
exclusiveMotions =
    [ ("w", repeat $ genMoveB unitViWord (Backward, InsideBound) Forward)
    , ("W", repeat $ genMoveB unitViWORD (Backward, InsideBound) Forward)
    , ("b", repeat $ moveB unitViWord Backward)
    , ("B", repeat $ moveB unitViWORD Backward)
    , ("^", const $ firstNonSpaceB)
    ]

inclusiveMotions :: [(String, Int -> BufferM ())]
inclusiveMotions =
    [ ("h", moveXorSol)
    , ("l", moveXorEol)
    , ("j", discard . lineMoveRel)
    , ("k", discard . lineMoveRel . negate)

    -- Word motions
    , ("e", repeat $ genMoveB unitViWord (Forward, InsideBound) Forward)
    , ("E", repeat $ genMoveB unitViWORD (Forward, InsideBound) Forward)
    , ("ge", repeat $ genMoveB unitViWord (Forward, InsideBound) Backward)
    , ("gE", repeat $ genMoveB unitViWORD (Forward, InsideBound) Backward)

    -- Intraline stuff
    , ("$", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol)
    ]

repeat :: BufferM () -> Int -> BufferM ()
repeat = flip replicateM_

regionOfMoveB :: CountedMove -> BufferM StyledRegion
regionOfMoveB (CountedMove n (Move style move)) = do 
    region <- mkRegion <$> pointB <*> destinationOfMoveB (move n)
    return $! StyledRegion style region
