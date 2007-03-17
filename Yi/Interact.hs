{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

{- |

Copyright   :  (c) The University of Glasgow 2002, (c) Jean-Philippe Bernardy 2007
License     :  BSD-style (see http:\/\/darcs.haskell.org\/packages\/base\/LICENSE)
Portability :  non-portable (local universal quantification)


This is a library of interactive processes combinators, usable to
define extensible keymaps. 

(This is based on Text.ParserCombinators.ReadP, originally written by Koen Claessen.)


The processes are:

* composable

* extensible: it is always possible to override a behaviour by using the '<++' operator

* monadic: sequencing is done via monadic bind. (leveraging the whole
  battery of monadic tools that Haskell provides)

The processes can parse input, and write output that depends on it.
The overall idea is that processes should produce output as soon as possible; so that
an execution of a process can be interactive.

The semantics of operation is therefore quite obvious; ony disjunction
deserve a bit more explanation:

@(a '+++' b)@ means

* if @a@ produces output earlier than @b@, we commit to the @a@ process (the converse is true)

* if both produce output at the same time (ie. after reading the same
  character in the input), then we commit to @a@ (ie. left bias)

Extensibility by overriding is achieved by the '<++' operator.

@(a '<++' b)@ will commit to @a@, unless it fails before producing any output.


-}

{-

Implementation notes:
 * Being based on Text.ParserCombinators.ReadP, these processes do not hold to input (ie. no memory leak)

-}


module Yi.Interact ( 
  -- * The 'Interact' type
  Interact,      -- :: * -> *; instance Functor, Monad, MonadPlus
  
  -- * Primitive operations
  MonadInteract (..),
  (+++),
  gather,     -- :: Interact a -> Interact (String, a)
  
  -- * Other operations
  oneOf,
  satisfy,    -- :: (Char -> Bool) -> Interact Char
  event,      -- :: Char -> Interact Char
  -- string,     -- :: String -> Interact String
  events,
  choice,     -- :: [Interact a] -> Interact a
  count,      -- :: Int -> Interact a -> Interact [a]
  between,    -- :: Interact open -> Interact close -> Interact a -> Interact a
  option,     -- :: a -> Interact a -> Interact a
  optional,   -- :: Interact a -> Interact ()
  many',
  many1',
  many,       -- :: Interact a -> Interact [a]
  many1,      -- :: Interact a -> Interact [a]
  skipMany,   -- :: Interact a -> Interact ()
  skipMany1,  -- :: Interact a -> Interact ()
  sepBy,      -- :: Interact a -> Interact sep -> Interact [a]
  sepBy1,     -- :: Interact a -> Interact sep -> Interact [a]
  endBy,      -- :: Interact a -> Interact sep -> Interact [a]
  endBy1,     -- :: Interact a -> Interact sep -> Interact [a]
  chainr,     -- :: Interact a -> Interact (a -> a -> a) -> a -> Interact a
  chainl,     -- :: Interact a -> Interact (a -> a -> a) -> a -> Interact a
  chainl1,    -- :: Interact a -> Interact (a -> a -> a) -> Interact a
  chainr1,    -- :: Interact a -> Interact (a -> a -> a) -> Interact a
  manyTill,   -- :: Interact a -> Interact end -> Interact [a]
  forever,
  -- * Running a parser
  runProcessParser, -- :: Interact a -> ReadS a
  runProcess, 
  
  -- * Properties
  -- $properties
  ) 
 where

import Control.Monad ( MonadPlus(..), sequence, liftM2 )
import Control.Monad.State hiding ( get )

import Yi.Keymap ( Action )
infixr 5 +++, <++

class (Monad m, MonadPlus m) => MonadInteract m e | m -> e where
    write :: Action -> m ()
    -- ^ Outputs an action.
    anyEvent :: m e
    -- ^ Consumes and returns the next character.
    --   Fails if there is no input left.
    (<++) :: m a -> m a -> m a
    -- ^ @(a '<++' b)@ will commit to @a@, unless it fails before producing any output.
    consumeLookahead :: m a -> m (Either [e] a)
    -- ^ Transforms a parser into one that does the same, except when it fails.
    --   In that case, it just consumes the the amount of characters demanded by its argument for it to fail.
    --   Typically this will be used in a "many" combinator

instance MonadInteract (Interact event) event where
    write = writeL
    anyEvent = get
    (<++) = mLeftPlusL
    consumeLookahead = consumeLookaheadL

-- Needs -fallow-undecidable-instances
instance MonadInteract m e => MonadInteract (StateT s m) e where
    write = lift . write
    anyEvent = lift anyEvent
    m <++ n = StateT $ \s -> runStateT m s <++ runStateT n s
    consumeLookahead m = StateT $ \s -> liftM (\r -> case r of 
                                                      Left e -> (Left e,s)
                                                      Right (a,s') -> (Right a,s')       
                                             ) (consumeLookahead (runStateT m s))

-- ---------------------------------------------------------------------------
-- The P type
-- is representation type -- should be kept abstract

data P event a
  = Get (event -> P event a)
  | Look Int ([event] -> P event a)
  | Fail
  | Result a (P event a)
  | Write Action (() -> P event a) -- TODO: Remove the dummy () parameter ?

-- Monad, MonadPlus

instance Monad (P event) where
  return x = Result x Fail

  (Get f)      >>= k = Get (\c -> f c >>= k)
  (Look n f)   >>= k = Look n (\s -> f s >>= k)
  Fail         >>= _ = Fail
  (Write w p)  >>= k = Write w (\u -> p u >>= k)
  (Result x p) >>= k = k x `mplus` (p >>= k)

  fail _ = Fail

instance MonadPlus (P event) where
  mzero = Fail

  -- In case of conflicting Write, we commit to the leftmost writer.
  Write w p  `mplus` _          = Write w p
  _          `mplus` Write w p  = Write w p

  -- most common case: two gets are combined
  Get f1     `mplus` Get f2     = Get (\c -> f1 c `mplus` f2 c)
  
  -- results are delivered as soon as possible
  Result x p `mplus` q          = Result x (p `mplus` q)
  p          `mplus` Result x q = Result x (p `mplus` q)

  -- fail disappears
  Fail       `mplus` p          = p
  p          `mplus` Fail       = p

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look n f   `mplus` Look m g   = Look (max n m) (\s -> f s `mplus` g s)
  Look n f   `mplus` p          = Look n (\s -> f s `mplus` p)
  p          `mplus` Look n f   = Look n (\s -> p `mplus` f s)
                                  
-- ---------------------------------------------------------------------------
-- The Interact type

newtype Interact event a = R (forall b . (a -> P event b) -> P event b)

-- Functor, Monad, MonadPlus

instance Functor (Interact event) where
  fmap h (R f) = R (\k -> f (k . h))

instance Monad (Interact event) where
  return x  = R (\k -> k x)
  fail _    = R (\_ -> Fail)
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance MonadPlus (Interact event) where
  mzero = pfail
  mplus = mplusL

-- ---------------------------------------------------------------------------
-- Operations over P

run :: P event a -> [event] -> [(a,[event])]
run (Get f)      (c:s) = run (f c) s
run (Look _ f)   s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Write _ p)  s     = run (p ()) s -- drop the written things in this version.
run _            _     = []


runWrite :: P event a -> [event] -> [Action]
runWrite (Get f)      (c:s) = runWrite (f c) s
runWrite (Look _ f)   s     = runWrite (f s) s
runWrite (Result _ p) s     = runWrite p s
runWrite (Write w p)  s     = w : runWrite (p ()) s
runWrite _            _     = []


-- | Returns the amount of demanded input for running the given parser.
consumed :: P event a -> [event] -> Int
consumed (Get f)       (c:s) = 1 + consumed (f c) s
consumed (Look (-1) _) _     = error "indefinite look is not supported by consumeLookahead"
consumed (Look n f)    s     = max n (consumed (f s) s)
consumed (Result _ p)  s     = consumed p s
consumed (Write _ p)   s     = consumed (p ()) s
consumed _             _     = 0

-- ---------------------------------------------------------------------------
-- Operations over Interact

writeL :: Action -> Interact event ()
writeL w = R (Write w)

get :: Interact event event
get = R Get

look :: Int -> Interact event [event]
-- ^ Look-ahead: returns the part of the input that is left, without
--   consuming it. @n@ chars will be demanded.
look n = R (Look n)

pfail :: Interact event a
-- ^ Always fails.
pfail = R (\_ -> Fail)

(+++) :: MonadInteract m e => m a -> m a -> m a
(+++) = mplus

mplusL :: Interact event a -> Interact event a -> Interact event a
-- ^ Symmetric choice.
R f1 `mplusL` R f2 = R (\k -> f1 k `mplus` f2 k)

mLeftPlusL :: Interact event a -> Interact event a -> Interact event a
R r `mLeftPlusL` q =
  do s <- look 0 -- look 0 bypasses the protection in 'consumed'; 
                 -- it's ok because we do more specific looks below.
     probe (r return) s 0
 where
  probe (Get f)        (c:s) n = probe (f c) s (n+1)
  probe (Look _ f)     s     n = probe (f s) s n
  probe p@(Result _ _) _     n = R (Look n) >> discard n >> R (p >>=)
  probe p@(Write  _ _) _     n = R (Look n) >> discard n >> R (p >>=)
  probe _              _     n = R (Look n) >> q

discard :: Int -> Interact event ()
discard 0 = return ()
discard n  = get >> discard (n-1)

consumeLookaheadL :: Interact event a -> Interact event (Either [event] a)
consumeLookaheadL (R f) = do
  s <- look (-1)
  case run (f return) s of    
    [] -> let n = consumed (f return) s in discard n >> return (Left (take n s))
    _ -> R f >>= return . Right

gather :: Interact event a -> Interact event ([event], a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
gather (R m) =
  R (\k -> gath id (m (\a -> return (\s -> k (s,a)))))  
 where
  gath l (Get f)      = Get (\c -> gath (l.(c:)) (f c))
  gath _ Fail         = Fail
  gath l (Look n f)   = Look n (\s -> gath l (f s))
  gath l (Result k p) = k (l []) `mplus` gath l p
  gath l (Write w p)  = Write w (\k -> gath l (p k))

-- ---------------------------------------------------------------------------
-- Derived operations
oneOf :: (Eq event, MonadInteract m event) => [event] -> m event
oneOf s = satisfy (`elem` s)

satisfy :: MonadInteract m event => (event -> Bool) -> m event
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do c <- anyEvent; if p c then return c else fail "not satisfy'ed"

event :: (Eq event, MonadInteract m event) => event -> m event
-- ^ Parses and returns the specified character.
event c = satisfy (c ==)

events :: (Eq event, MonadInteract m event) => [event] -> m [event]
-- ^ Parses and returns the specified list of events (lazily). 
events = mapM event

choice :: (MonadInteract m e) => [m a] -> m a
-- ^ Combines all parsers in the specified list.
choice []     = fail "No choice succeeds"
choice [p]    = p
choice (p:ps) = p +++ choice ps

count :: Int -> Interact event a -> Interact event [a]
-- ^ @count n p@ parses @n@ occurrences of @p@ in sequence. A list of
--   results is returned.
count n p = sequence (replicate n p)

between :: Interact event open -> Interact event close -> Interact event a -> Interact event a
-- ^ @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between open close p = do open
                          x <- p
                          close
                          return x

option :: (MonadInteract m e) => a -> m a -> m a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p +++ return x

optional :: (MonadInteract m e) => m a -> m ()
-- ^ @optional p@ optionally parses @p@ and always returns @()@.
optional p = (p >> return ()) +++ return ()


many :: (MonadInteract m e) => m a -> m [a]
-- ^ Parses zero or more occurrences of the given parser.
many p = return [] +++ many1 p

many1 :: (MonadInteract m e) => m a -> m [a]
-- ^ Parses one or more occurrences of the given parser.
many1 p = liftM2 (:) p (many p)

many' :: (MonadInteract m e) => m a -> m [a]
-- ^ Parses zero or more occurrences of the given parser.
-- ^ Same as 'many', but the preference is to the given parser instead of leaving the loop, using '<++'.
many' p = many1' p <++ return []

many1' :: (MonadInteract m e) => m a -> m [a]
-- ^ Parses one or more occurrences of the given parser.
-- ^ Same as 'many1', but the preference is to the given parser instead of leaving the loop, using '<++'.
many1' p = liftM2 (:) p (many' p)


skipMany :: (MonadInteract m e) => m a -> m ()
-- ^ Like 'many', but discards the result.
skipMany p = many p >> return ()

skipMany1 :: (MonadInteract m e) => m a -> m ()
-- ^ Like 'many1', but discards the result.
skipMany1 p = p >> skipMany p

sepBy :: (MonadInteract m e) => m a -> m sep -> m [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: (MonadInteract m e) => m a -> m sep -> m [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: (MonadInteract m e) => m a -> m sep -> m [a]
-- ^ @endBy p sep@ parses zero or more occurrences of @p@, separated and ended
--   by @sep@.
endBy p sep = many (do x <- p ; sep ; return x)

endBy1 :: (MonadInteract m e) => m a -> m sep -> m [a]
-- ^ @endBy p sep@ parses one or more occurrences of @p@, separated and ended
--   by @sep@.
endBy1 p sep = many1 (do x <- p ; sep ; return x)

chainr :: (MonadInteract m e) => m a -> m (a -> a -> a) -> a -> m a
-- ^ @chainr p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /right/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainr p op x = chainr1 p op +++ return x

chainl :: (MonadInteract m e) => m a -> m (a -> a -> a) -> a -> m a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op +++ return x

chainr1 :: (MonadInteract m e) => m a -> m (a -> a -> a) -> m a
-- ^ Like 'chainr', but parses one or more occurrences of @p@.
chainr1 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 +++ return x

chainl1 :: (MonadInteract m e) => m a -> m (a -> a -> a) -> m a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 +++ return x

manyTill :: (MonadInteract m e) => m a -> m end -> m [a]
-- ^ @manyTill p end@ parses zero or more occurrences of @p@, until @end@
--   succeeds. Returns a list of values returned by @p@.
manyTill p end = scan
  where scan = (end >> return []) <++ (liftM2 (:) p scan)

forever :: Monad m => m a -> m b
forever f = f >> forever f

-- ---------------------------------------------------------------------------
-- Converting between Interact event and Read

runProcessParser :: Interact event a -> [event] -> [(a,[event])]
-- ^ Converts a process into a Haskell ReadS-style parsing function. (for debugging)
runProcessParser (R f) = run (f return)



runProcess :: Interact event a -> [event] -> [Action]
-- ^ Converts a process into a function that maps input to output.
-- The process does not hold to the input stream (no space leak) and
-- produces the output as soon as possible.
runProcess (R f) = runWrite (f return)

