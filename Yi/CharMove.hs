--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--               2004 Tuomo Valkonen
--
--

--
-- | Char-based movement actions.
--
module Yi.CharMove (
        -- * Moving to a specific character
        nextCInc,       -- :: Char -> BufferM ()
        nextCExc,       -- :: Char -> BufferM ()
        prevCInc,       -- :: Char -> BufferM ()
        prevCExc,       -- :: Char -> BufferM ()

        firstNonSpaceB
    ) where

import Yi.Buffer 
import Text.Regex
import Yi.Dynamic
import Data.Char
import Data.Typeable
import qualified Data.Map as M

import Control.Monad        ( when, replicateM_ )
import Control.Applicative
import Control.Monad.Fix    ( fix )
import Control.Exception    ( assert )
import Yi.Buffer.Normal
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.String


-- | Shift the point, until predicate is true, leaving point at final
-- location.
moveWhileB :: (Char -> Bool) -> Direction -> BufferM ()
moveWhileB f dir = do
    eof <- sizeB
    case dir of
        Forward   -> fix $ \loop' -> do 
                       p <- pointB
                       when (p < eof - 1) $ do
                                       x <- readB
                                       when (f x) $ rightB >> loop'
        Backward -> fix $ \loop' -> do 
                       p <- pointB
                       when (p > 0) $ do
                                       x <- readB
                                       when (f x) $ leftB >> loop'


-- | Move to the next occurence of @c@
nextCInc :: Char -> BufferM ()
nextCInc c = rightB >> moveWhileB (/= c) Forward

-- | Move to the character before the next occurence of @c@
nextCExc :: Char -> BufferM ()
nextCExc c = nextCInc c >> leftB

-- | Move to the previous occurence of @c@
prevCInc :: Char -> BufferM ()
prevCInc c = leftB  >> moveWhileB (/= c) Backward

-- | Move to the character after the previous occurence of @c@
prevCExc :: Char -> BufferM ()
prevCExc c = prevCInc c >> rightB

-- | Move to first non-space character in this line
firstNonSpaceB :: BufferM ()
firstNonSpaceB = do
        moveToSol
        fix $ \loop -> do
            eol <- atEol
            if eol then return ()
                   else do k <- readB
                           when (isSpace k) (rightB >> loop)



