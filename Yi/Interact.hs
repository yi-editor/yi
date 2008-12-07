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
     I, P (End),
     InteractState (..),
     MonadInteract (..),
     PEq (..),
     deprioritize,
     (<||),
     option,
     oneOf,
     processOneEvent,
     computeState,
     event,
     events,
     choice,
     mkAutomaton,
     runWrite,
     anyEvent,
     eventBetween,
    ) where

import Control.Arrow (first)
import Control.Monad.State hiding ( get, mapM )
import Data.Monoid
import Yi.Prelude
import Prelude ()
import Data.Maybe
import Data.List (filter, map, groupBy)
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
    adjustPriority :: Int -> m ()


-------------------------------------------------
-- State transformation

-- Needs -fallow-undecidable-instances
-- TODO: abstract over MonadTransformer
instance MonadInteract m w e => MonadInteract (StateT s m) w e where
    write = lift . write
    eventBounds l h = lift (eventBounds l h)
    adjustPriority p = lift (adjustPriority p)

instance (MonadInteract m w e) => Alternative (StateT s m) where
    empty = mzero
    (<|>) = mplus

instance MonadInteract m w e => Applicative (StateT s m) where
    pure = return
    a <*> b = do f <- a; x <- b; return (f x)

---------------------------------------------------------------------------
-- | Interactive process description

-- TODO: Replace 'Doc:' by ^ when haddock supports GADTs
data I ev w a where
    Returns :: a -> I ev w a
    Binds :: I ev w a -> (a -> I ev w b) -> I ev w b
    Gets :: Ord ev => Maybe ev -> Maybe ev -> I ev w ev
    -- Doc: Accept any character between given bounds. Bound is ignored if 'Nothing'.
    Fails :: I ev w a
    Writes :: w -> I ev w ()
    Priority :: Int -> I ev w ()
    Plus :: I ev w a -> I ev w a -> I ev w a


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
    write = Writes
    eventBounds = Gets
    adjustPriority dp = Priority dp


infixl 3 <||

deprioritize :: (MonadInteract f w e) => f ()
deprioritize = adjustPriority 1

(<||) :: (MonadInteract f w e) => f a -> f a -> f a
a <|| b = a <|> (deprioritize >> b)



-- | Convert a process description to an "executable" process.
mkProcess :: PEq w => I ev w a -> ((a -> P ev w) -> P ev w)
mkProcess (Returns x) = \fut -> fut x
mkProcess Fails = (\_fut -> Fail)
mkProcess (m `Binds` f) = \fut -> (mkProcess m) (\a -> mkProcess (f a) fut)
mkProcess (Gets l h) = Get l h
mkProcess (Writes w) = \fut -> Write w (fut ())
mkProcess (Priority p) = \fut -> Prior p (fut ())
mkProcess (Plus a b) = \fut -> Best (mkProcess a fut) (mkProcess b fut)


----------------------------------------------------------------------
-- Process type

-- | Operational representation of a process
data P event w
    = Ord event => Get (Maybe event) (Maybe event) (event -> P event w)
    | Fail
    | Write w (P event w)
    | Prior Int (P event w) -- low numbers indicate high priority
    | Best (P event w) (P event w)
    | End

-- ---------------------------------------------------------------------------
-- Operations over P

runWrite :: PEq w => P event w -> [event] -> [w]
runWrite _ [] = []
runWrite p (c:cs) = let (ws, p') = processOneEvent p c in ws ++ runWrite p' cs

processOneEvent :: PEq w => P event w -> event -> ([w], P event w)
processOneEvent p e = pullWrites $ pushEvent p e

-- | Push an event in the automaton
pushEvent :: P ev w -> ev -> P ev w
pushEvent (Best c d) e = Best (pushEvent c e) (pushEvent d e)
pushEvent (Write w c) e = Write w (pushEvent c e)
pushEvent (Prior p c) e = Prior p (pushEvent c e)
pushEvent (Get l h f) e = if test (e >=) l && test (e <=) h then f e else Fail
    where test = maybe True
pushEvent Fail _ = Fail
pushEvent End _ = End

-- | Abstraction of the automaton state.
data InteractState event w =  Ambiguous [(Int,w,P event w)] | Waiting | Dead | Running w (P event w)

instance Monoid (InteractState event w) where
    -- not used at the moment:
    mappend (Running w c) _ = Running w c
    mappend _ (Running w c) = Running w c
    -- don't die if that can be avoided
    mappend Dead p = p
    mappend p Dead = p
    -- If a branch is not determined, wait for it.
    mappend Waiting _ = Waiting
    mappend _ Waiting = Waiting
    -- ambiguity remains
    mappend (Ambiguous a) (Ambiguous b) = Ambiguous (a ++ b)
    mempty = Ambiguous []
    

-- | find all the writes that are accessible.
findWrites :: Int -> P event w -> InteractState event w
findWrites p (Best c d) = findWrites p c `mappend` findWrites p d
findWrites p (Write w c) = Ambiguous [(p,w,c)]
findWrites p (Prior dp c) = findWrites (p+dp) c
findWrites _ Fail = Dead
findWrites _ End = Dead
findWrites _ (Get _ _ _) = Waiting


computeState :: PEq w => P event w -> InteractState event  w
computeState a = case findWrites 0 a of
    Ambiguous actions -> let prior = minimum $ map fst3 $ actions
                             bests = groupBy (equiv `on` snd3) $ filter ((prior ==) . fst3) actions
                         in case bests of
                              [((_,w,c):_)] -> Running w c
                              _ -> Ambiguous $ map head bests
    s -> s
                           
                           

pullWrites :: PEq w => P event w -> ([w], P event w)
pullWrites a = case computeState a of
    Running w c -> first (w:) (pullWrites c)
    _ -> ([], a)


instance (Show w, Show ev) => Show (P ev w) where
    show (Get Nothing Nothing _) = "?"
    show (Get (Just l) (Just h) _p) | l == h = show l -- ++ " " ++ show (p l)
    show (Get l h _) = maybe "" show l ++ ".." ++ maybe "" show h
    show (Prior p c) = ":" ++ show p ++ show c
    show (Write w c) = "!" ++ show w ++ "->" ++ show c
    show (End) = "."
    show (Fail) = "*"
    show (Best p q) = "{" ++ show p ++ "|" ++ show q ++ "}"

-- ---------------------------------------------------------------------------
-- Derived operations
oneOf :: (Ord event, MonadInteract m w event) => [event] -> m event
oneOf s = choice $ map event s

anyEvent :: (Ord event, MonadInteract m w event) => m event
anyEvent = eventBounds Nothing Nothing

eventBetween :: (Ord e, MonadInteract m w e) => e -> e -> m e
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



