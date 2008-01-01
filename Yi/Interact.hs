{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

{- |

Copyright   :  (c) The University of Glasgow 2002, (c) Jean-Philippe Bernardy 2007
License     :  BSD-style (see http:\/\/darcs.haskell.org\/packages\/base\/LICENSE)
Portability :  non-portable (local universal quantification)


This is a library of interactive processes combinators, usable to
define extensible keymaps. 

(Inspired by the Parsek library, written by Koen Claessen)

The processes are:

* composable

* extensible: it is always possible to override a behaviour by combination of 
  'deprioritize' and '<|>'. (See also '<||' for a convenient combination of the two.)

* monadic: sequencing is done via monadic bind. (leveraging the whole
  battery of monadic tools that Haskell provides)

The processes can parse input, and write output that depends on it.

The semantics are quite obvious; ony disjunction
deserve a bit more explanation:

in @p = (a '<|>' b)@, what happens if @a@ and @b@ recognize the same
input (prefix), but produce conflicting output?

* if the output is the same (as by the PEq class), then the processes (prefixes) are "merged"
* if a Write is more prioritized than the other, the one with low priority will be discarded
* otherwise, the output will be delayed until one of the branches can be discarded.
* if there is no way to disambiguate, then no output will be generated anymore. (oops!)



-}

module Yi.Interact 
    (
     I, P,
     MonadInteract (..),
     PEq (..),
     (<||),
     comap,
     option,
     oneOf,
     runProcess,
     event,
     events,
     satisfy,
     choice
    ) where

import Control.Applicative
import Control.Monad.State hiding ( get )

------------------------------------------------
-- Classes

class PEq a where
    equiv :: a -> a -> Bool
-- | Abstraction of monadic interactive processes
class (PEq w, Monad m, Alternative m, Applicative m, MonadPlus m) => MonadInteract m w e | m -> w e where
    write :: w -> m ()
    -- ^ Outputs a result.
    anyEvent :: m e
    -- ^ Consumes and returns the next character.
    --   Fails if there is no input left.
    deprioritize :: m a -> m a


-------------------------------------------------
-- State transformation

-- Needs -fallow-undecidable-instances
instance MonadInteract m w e => MonadInteract (StateT s m) w e where
    write = lift . write
    anyEvent = lift anyEvent
    deprioritize (StateT f) = StateT (\s -> deprioritize (f s))
                         
instance (MonadInteract m w e) => Alternative (StateT s m) where
    empty = mzero
    (<|>) = mplus

instance MonadInteract m w e => Applicative (StateT s m) where
    pure = return
    a <*> b = do f <- a; x <- b; return (f x)


---------------------------------------------------------------------------
-- | Interactive process description
data I ev w a where
    Returns :: a -> I ev w a
    Binds :: I ev w a -> (a -> I ev w b) -> I ev w b
    Gets :: I ev w ev
    Fails :: I ev w a
    Writes :: Int -> w -> I ev w ()
    Plus :: I ev w a -> I ev w a -> I ev w a


                   
-- | Cofunctor on the the event parameter. This can be used to convert
-- from a specific event type to a general one. (e.g. from
-- full-fleged events to chars)

comap :: (ev1 -> ev2) -> I ev2 m a -> I ev1 m a 
comap _f (Returns a) = Returns a
comap f Gets = fmap f Gets
comap f (Binds a b) = Binds (comap f a) (comap f . b)
comap f Fails = Fails
comap f (Writes p w) = Writes p w
comap f (Plus a b) = Plus (comap f a) (comap f b)


instance Functor (I event w) where
  fmap f i = pure f <*> i

instance Applicative (I ev w) where
    pure = return
    a <*> b = do f <- a; x <- b; return (f x)

instance Alternative (I ev w) where
    empty = Fails
    (<|>) = Plus

instance Monad (I event w) where
  return  = Returns
  fail _  = Fails
  (>>=)   = Binds

instance PEq w => MonadPlus (I event w) where
  mzero = Fails
  mplus = Plus

instance PEq w => MonadInteract (I event w) w event where
    write = Writes 0
    anyEvent = Gets
    deprioritize i = case i of
      Returns x -> Returns x
      Binds x f -> Binds (deprioritize x) (\y -> deprioritize (f y)) 
      Gets -> Gets
      Fails -> Fails
      Writes p w -> Writes (p + 1) w
      Plus a b -> Plus (deprioritize a) (deprioritize b)




infixl 3 <||

(<||) :: (MonadInteract f w e) => f a -> f a -> f a
a <|| b = a <|> (deprioritize b)



-- | Convert a process description to an "executable" process.
mkProcess :: PEq w => I ev w a -> ((a -> P ev w) -> P ev w)
mkProcess (Returns x) = \fut -> fut x
mkProcess Fails = (\_fut -> Fail) 
mkProcess (m `Binds` f) = \fut -> (mkProcess m) (\a -> mkProcess (f a) fut)
mkProcess Gets = Get
mkProcess (Writes prior w) = \fut -> Write prior w (fut ())
mkProcess (Plus a b) = \fut -> best (mkProcess a fut) (mkProcess b fut)


----------------------------------------------------------------------
-- Process type

-- | Operational representation of a process
data P event w
    = Get (event -> P event w)
    | Fail
    | Write Int w (P event w)  -- low numbers indicate high priority


-- | Merge two processes so they run in parallel.
best :: PEq w => P ev w -> P ev w -> P ev w
Write p w c  `best` Write q x d
-- Prioritized write:
    | p < q = Write p w c
    | p > q = Write q x d
-- Agreeing writes:
    | equiv w x = Write p w (best c d)
-- (Disagreeing writes will be delayed)

-- two gets are combined
Get f1     `best` Get f2     = Get (\c -> f1 c `best` f2 c)

-- fail disappears
Fail       `best` p          = p
p          `best` Fail       = p

-- otherwise, bring Get or Fail to the top in both processes and continue.
best p q = progress id p `best` progress id q 

-- | Progress in the input, delaying the writes.
progress :: (P ev w -> P ev w) -> (P ev w -> P ev w)
progress f (Get s) = Get (\ev -> f (s ev))
progress _f (Fail)  = Fail
progress f (Write prior w s) = progress (\fut -> f (Write prior w fut)) s


-- ---------------------------------------------------------------------------
-- Operations over P

runWrite :: P event w -> [event] -> [w]
runWrite (Get f)      (c:s) = runWrite (f c) s
runWrite (Get _f)      []    = []
runWrite (Write _ w p)  s   = w : runWrite p s
runWrite Fail         _s     = []

stepProcess :: P event w -> [event] -> (Maybe w, P event w, [event])
stepProcess (Get f)      (c:s) = (Nothing, f c, s)
stepProcess (Get f)      []    = (Nothing, Fail, [])
stepProcess (Write _ w p)  s     = (Just w, p, s)
stepProcess Fail         s     = (Nothing, Fail, s)



run :: P event w -> [event] -> [P event w]
run p s = p:let (_,p',s') = stepProcess p s in run p' s'


instance (Show w, Show ev) => Show (P ev w) where
    show (Get f) = "?"
    show (Write prior w p) = "!" ++ show prior ++ ":" ++ show w
    show (Fail) = "."

-- ---------------------------------------------------------------------------
-- Derived operations
oneOf :: (Eq event, MonadInteract m w event) => [event] -> m event
oneOf s = satisfy (`elem` s)

satisfy :: MonadInteract m w event => (event -> Bool) -> m event
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do c <- anyEvent; if p c then return c else fail "not satisfy'ed"

event :: (Eq event, MonadInteract m w event) => event -> m event
-- ^ Parses and returns the specified character.
event c = satisfy (c ==)

events :: (Eq event, MonadInteract m w event) => [event] -> m [event]
-- ^ Parses and returns the specified list of events (lazily). 
events = mapM event

choice :: (MonadInteract m w e) => [m a] -> m a
-- ^ Combines all parsers in the specified list.
choice []     = fail "No choice succeeds"
choice [p]    = p
choice (p:ps) = p `mplus` choice ps



-- ^ @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between open close p = do open
                          x <- p
                          close
                          return x

option :: (MonadInteract m w e) => a -> m a -> m a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p `mplus` return x

skipMany :: (MonadInteract m w e) => m a -> m ()
-- ^ Like 'many', but discards the result.
skipMany p = many p >> return ()

skipMany1 :: (MonadInteract m w e) => m a -> m ()
-- ^ Like 'many1', but discards the result.
skipMany1 p = p >> skipMany p

sepBy :: (MonadInteract m w e) => m a -> m sep -> m [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep `mplus` return []

sepBy1 :: (MonadInteract m w e) => m a -> m sep -> m [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: (MonadInteract m w e) => m a -> m sep -> m [a]
-- ^ @endBy p sep@ parses zero or more occurrences of @p@, separated and ended
--   by @sep@.
endBy p sep = many (do x <- p ; sep ; return x)

-- ^ @endBy p sep@ parses one or more occurrences of @p@, separated and ended
--   by @sep@.
endBy1 p sep = some (do x <- p ; sep ; return x)

chainr :: (MonadInteract m w e) => m a -> m (a -> a -> a) -> a -> m a
-- ^ @chainr p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /right/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainr p op x = chainr1 p op `mplus` return x

chainl :: (MonadInteract m w e) => m a -> m (a -> a -> a) -> a -> m a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op `mplus` return x

chainr1 :: (MonadInteract m w e) => m a -> m (a -> a -> a) -> m a
-- ^ Like 'chainr', but parses one or more occurrences of @p@.
chainr1 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 `mplus` return x

chainl1 :: (MonadInteract m w e) => m a -> m (a -> a -> a) -> m a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 `mplus` return x

manyTill :: (MonadInteract m w e) => m a -> m end -> m [a]
-- ^ @manyTill p end@ parses zero or more occurrences of @p@, until @end@
--   succeeds. Returns a list of values returned by @p@.
manyTill p end = scan
  where scan = (end >> return []) `mplus` (liftM2 (:) p scan)


runProcess :: PEq w => I event w a -> [event] -> [w]
-- ^ Converts a process into a function that maps input to output.
-- The process does not hold to the input stream (no space leak) and
-- produces the output as soon as possible.
runProcess f = runWrite (mkP f)

runP :: PEq w => I event w a -> [event] -> [P event w]
runP f = run (mkP f)

mkP i = mkProcess i (const (Fail))

mkF i = mkProcess i (const (Get (\c -> Fail)))

