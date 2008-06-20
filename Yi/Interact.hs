{-# LANGUAGE UndecidableInstances, GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

-- Copyright (c) Jean-Philippe Bernardy 2007-8

{- |
This is a library of interactive processes combinators, usable to
define extensible keymaps.

(Inspired by the Parsec library, written by Koen Claessen)

The processes are:

* composable: in parallel using '<|>', in sequence using monadic bind.

* extensible: it is always possible to override a behaviour by combination of
  'adjustPriority' and '<|>'. (See also '<||' for a convenient combination of the two.)

* monadic: sequencing is done via monadic bind. (leveraging the whole
  battery of monadic tools that Haskell provides)

The processes can parse input, and write output that depends on it.

The semantics are quite obvious; only disjunction
deserve a bit more explanation:

in @p = (a '<|>' b)@, what happens if @a@ and @b@ recognize the same
input (prefix), but produce conflicting output?

* if the output is the same (as by the PEq class), then the processes (prefixes) are "merged"
* if a Write is more prioritized than the other, the one with low priority will be discarded
* otherwise, the output will be delayed until one of the branches can be discarded.
* if there is no way to disambiguate, then no output will be generated anymore. 
  This situation can be detected by using 'possibleActions' however.
-}

module Yi.Interact
    (
     I, P (Fail, End),
     MonadInteract (..),
     PEq (..),
     deprioritize,
     (<||),
     comap,
     option,
     oneOf,
     processOneEvent,
     possibleActions,
     event,
     events,
     choice,
     mkAutomaton,
     runWrite,
     anyEvent,
     eventBetween,
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State hiding ( get )
import Data.List (nubBy)

------------------------------------------------
-- Classes

class PEq a where
    equiv :: a -> a -> Bool

-- | Abstraction of monadic interactive processes
class (PEq w, Monad m, Alternative m, Applicative m, MonadPlus m) => MonadInteract m w e | m -> w e where
    write :: w -> m ()
    -- ^ Outputs a result.
    eventBounds :: Ord e => Maybe e -> Maybe e -> m e
    -- ^ Consumes and returns the next character.
    --   Fails if there is no input left, or outside the given bounds.
    adjustPriority :: Int -> m a -> m a


-------------------------------------------------
-- State transformation

-- Needs -fallow-undecidable-instances
instance MonadInteract m w e => MonadInteract (StateT s m) w e where
    write = lift . write
    eventBounds l h = lift (eventBounds l h)
    adjustPriority p (StateT f) = StateT (\s -> adjustPriority p (f s))

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
    Gets :: Ord ev => Maybe ev -> Maybe ev -> I ev w ev
    -- ^ Accept any character between given bounds. Bound is ignored if 'Nothing'.
    Fails :: I ev w a
    Writes :: Int -> w -> I ev w ()
    Plus :: I ev w a -> I ev w a -> I ev w a



-- | Functor on the the event parameter. This can be used to convert
-- from a specific event type to a general one. (e.g. from
-- full-fleged events to chars)

comap :: Ord ev1 => (ev2 -> ev1) -> (ev1 -> ev2) -> I ev2 m a -> I ev1 m a
comap _f' _f (Returns a) = Returns a
comap  f'  f (Gets l h) = fmap f (Gets (fmap f' l) (fmap f' h))
comap  f'  f (Binds a b) = Binds (comap f' f a) (comap f' f . b)
comap _f' _f Fails = Fails
comap _f' _f (Writes p w) = Writes p w
comap  f'  f (Plus a b) = Plus (comap f' f a) (comap f' f b)


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
    eventBounds = Gets
    adjustPriority dp i = case i of
      Returns x -> Returns x
      Binds x f -> Binds (adjustPriority dp x) (\y -> adjustPriority dp (f y))
      Gets l h -> Gets l h
      Fails -> Fails
      Writes p w -> Writes (p + dp) w
      Plus a b -> Plus (adjustPriority dp a) (adjustPriority dp b)




infixl 3 <||

deprioritize :: (MonadInteract f w e) => f a -> f a
deprioritize = adjustPriority 1

(<||) :: (MonadInteract f w e) => f a -> f a -> f a
a <|| b = a <|> (adjustPriority 1 b)



-- | Convert a process description to an "executable" process.
mkProcess :: PEq w => I ev w a -> ((a -> P ev w) -> P ev w)
mkProcess (Returns x) = \fut -> fut x
mkProcess Fails = (\_fut -> Fail)
mkProcess (m `Binds` f) = \fut -> (mkProcess m) (\a -> mkProcess (f a) fut)
mkProcess (Gets l h) = Get l h
mkProcess (Writes prior w) = \fut -> Write prior w (fut ())
mkProcess (Plus a b) = \fut -> Best (mkProcess a fut) (mkProcess b fut)


----------------------------------------------------------------------
-- Process type

-- | Operational representation of a process
data P event w
    = Ord event => Get (Maybe event) (Maybe event) (event -> P event w)
    | Fail
    | Write Int w (P event w)  -- low numbers indicate high priority
    | Best (P event w) (P event w)
    | End

-- ---------------------------------------------------------------------------
-- Operations over P

runWrite :: PEq w => P event w -> [event] -> [w]
runWrite _ [] = []
runWrite Fail _ = []
runWrite End _ = []
runWrite p (c:cs) = let (ws, p') = processOneEvent p c in ws ++ runWrite p' cs

processOneEvent :: PEq w => P event w -> event -> ([w], P event w)
processOneEvent p e = pullWrites $ simplify $ pushEvent p e

pushEvent :: P ev w -> ev -> P ev w

pushEvent (Best c d) e = Best (pushEvent c e) (pushEvent d e)
pushEvent (Write p w c) e = Write p w (pushEvent c e)
pushEvent (Get l h f) e = if test (e >=) l && test (e <=) h then f e else Fail
    where test f Nothing = True
          test f (Just x) = f x
pushEvent Fail _ = Fail
pushEvent End _ = End

-- | Remove failing cases; fuse writes;
simplify :: PEq w => P ev w -> P ev w
simplify (Best c d) = best' c d
simplify (Write p w c) = case simplify c of
      Fail -> Fail
      c' -> Write p w c'
simplify p = p

best' :: PEq w => P ev w -> P ev w -> P ev w
best' c d = best (simplify c) (simplify d)

-- | Pull the most writes from two parallel processes; both are guaranteed not to contain any "Best"
best :: PEq w => P ev w -> P ev w -> P ev w
Write p w c  `best` Write q x d
-- Prioritized write:
    | p < q = Write p w c
    | p > q = Write q x d
-- Agreeing writes:
    | equiv w x = Write p w (best' c d)
-- (Disagreeing writes will be delayed)

-- fail disappears
Fail       `best` p          = p
p          `best` Fail       = p

-- impossible to pull anything; leave Best constructor
p          `best` q = Best p q

pullWrites :: P event w -> ([w], P event w)
pullWrites (Write _ w p) = first (w:) (pullWrites p)
pullWrites p = ([], p)

-- | Return the list of possible actions to write.
-- 'Nothing' indicates a read.
possibleActions :: P event w -> [Maybe w]
possibleActions = nubBy equiv' . helper
    where helper (Get _ _ _) = [Nothing]
          helper (Write _ w _) = [Just w]
          helper (Best p q) = helper p ++ helper q
          helper _ = []
          equiv' Nothing Nothing = True
          equiv' _ _ = False

instance (Show w, Show ev) => Show (P ev w) where
    show (Get Nothing Nothing p) = "?"
    show (Get (Just l) (Just h) p) | l == h = show l -- ++ " " ++ show (p l)
    show (Get l h p) = maybe "" show l ++ ".." ++ maybe "" show h
    show (Write prior w p) = "!" ++ show prior ++ ":" ++ show w ++ "->" ++ show p
    show (End) = "."
    show (Fail) = "*"
    show (Best p q) = "{" ++ show p ++ "|" ++ show q ++ "}"

-- ---------------------------------------------------------------------------
-- Derived operations
oneOf :: (Ord event, Bounded event, MonadInteract m w event) => [event] -> m event
oneOf s = satisfy (`elem` s)

anyEvent :: (Ord event, MonadInteract m w event) => m event
anyEvent = eventBounds Nothing Nothing

satisfy :: (Ord event, Bounded event, MonadInteract m w event) => 
           (event -> Bool) -> m event
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do c <- anyEvent; if p c then return c else fail "not satisfy'ed"

eventBetween l h = eventBounds (Just l) (Just h)

event :: (Ord event, MonadInteract m w event) => event -> m event
-- ^ Parses and returns the specified character.
event e = eventBetween e e

events :: (Ord event, MonadInteract m w event) => [event] -> m [event]
-- ^ Parses and returns the specified list of events (lazily).
events = mapM event

choice :: (MonadInteract m w e) => [m a] -> m a
-- ^ Combines all parsers in the specified list.
choice []     = fail "No choice succeeds"
choice [p]    = p
choice (p:ps) = p `mplus` choice ps

option :: (MonadInteract m w e) => a -> m a -> m a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p `mplus` return x

-- runProcess :: PEq w => I event w a -> [event] -> [w]
-- -- ^ Converts a process into a function that maps input to output.
-- -- The process does not hold to the input stream (no space leak) and
-- -- produces the output as soon as possible.
-- runProcess f = runWrite (mkF f)
--     where mkF i = mkProcess i (const (Get (const Fail)))

mkAutomaton :: PEq w => I ev w a -> P ev w
mkAutomaton i = mkProcess i (const End)



