{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
--
-- Copyright (C) 2007 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

-- | This module defines a common interface for syntax highlighters.
--
-- Yi syntax highlighters are expressed as explicit lazy computations
-- of type 'Highlighter a' below; this type is effectively isomorphic
-- to [Char] -> [Style], but are explicitly lazy to admit safe fast uses.
--

module Yi.Syntax ( Highlighter(..)
                 , ExtHL(..)
                 ) where

import Yi.Style
import qualified Data.ByteString.Lazy.Char8 as LB


    

-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.
--
-- Highlighters currently directly use the Vty color types; among other
-- things, this prevents the gtk port from using synhl.
data Highlighter a = SynHL 
                           { hlStartState :: a -- ^ The start state for the highlighter.
                             -- | Colorize a block of data passed in as a ByteString,
                             -- returning the new state and any attributes produced.
                             -- This *must* be implementable as a `B.foldl'.
                           , hlColorize :: LB.ByteString -> a -> (a, [(Int,Style)])
                             -- | Colorize the end of file; this exists only to inform
                             -- states that lookahead will never happen.
                           , hlColorizeEOF :: a -> [(Int,Style)]
                           }

data ExtHL = forall a. Eq a => ExtHL (Maybe (Highlighter a))
