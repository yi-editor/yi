-- Copyright (c) JP Bernardy 2008
{-# OPTIONS -fglasgow-exts #-}
module Yi.IncrementalParse (Process, Void, 
                            recoverWith, symbol, eof, lookNext, runPolish, 
                            runP, profile, pushSyms, pushEof, evalR,
                            P, AlexState (..), scanner) where
import Yi.Lexer.Alex (AlexState (..))
import Yi.Prelude
import Prelude (Ordering(..))
import Yi.Syntax
import Data.List hiding (map, minimumBy)
import Data.Char
import Data.Maybe (listToMaybe)

{- ----------------------------------------

- Based on a mix between "Polish Parsers, Step by Step (Hughes and Swierstra)", 
  and "Parallel Parsing Processes (Claessen)"
  
  It's strongly advised to read the papers! :)
- The parser has "online" behaviour.

  This is a big advantage because we don't have to parse the whole file to
  begin syntax highlight the beginning of it.

- Basic error correction

- Based on Applicative functors.

  This is not as powerful as Monadic parsers, but easier to work with. This is
  needed if we want to build the result lazily.


-------------------------------------------}



data Void

type Process s a = Steps s a (Steps s Void Void)

-- | Our parsing processes.
-- To understand the design of this data type it is important to consider the
-- basic design goal: Our parser should return a (partial) result as soon as
-- possible, that is, as soon as only one of all possible parses of an input
-- can succeed.  This also means we want to be able to return partial results.
-- We therefore have to transform our parse tree into a linearized form that
-- allows us to return parts of it as we parse them.  Consider the following
-- data type:
--
-- > data BinTree = Node BinTree BinTree | Leaf Int
-- > ex1 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
--
-- Provided we know the arity of each constructor, we can unambiguously
-- represent @ex1@ (without using parentheses to resolve ambiguity) as:
--
-- > Node Leaf 1 Node Leaf 2 Leaf 3
--
-- This is simply a pre-order printing of the tree type and, in this case, is
-- exactly how we defined @ex1@ without all the parentheses.  It would,
-- however, be unnecessarily complicated to keep track of the arity of each
-- constructor, so we use a well-known trick: currying.  Note, that the
-- original definition of @ex1@ is actually a shorter notation for
--
-- > ((Node $ (Leaf $ 1)) $ ((Node $ (Leaf $ 2)) $ (Leaf $ 3)))
--
-- or as a tree
-- 
-- >                      $
-- >        .-------------'----------------------.
-- >        $                                    $
-- >     .--'-------.              .-------------'-------.
-- >   Node         $              $                     $
-- >             .--'-.         .--'-------.          .--'-.
-- >           Leaf   1       Node         $        Leaf   3
-- >                                    .--'-.
-- >                                  Leaf   2
--
-- where @$@ represents function application.  We can print this tree in
-- prefix-order:
--
-- > ($) ($) Node ($) Leaf 1 ($) ($) Node ($) Leaf 2 ($) Leaf 3
--
-- This consists of only two types of nodes -- values and applications -- but
-- we can construct values of any (non-strict) Haskell data type with it.
--
-- Unfortunately, it is a bit tricky to type those kinds of expressions in
-- Haskell.  [XXX: example; develop solution step by step; continuations]
--
-- The parameter @r@ represents the type of the remainder of our expression.

-- TODO: Replace 'Doc:' by ^ when haddock supports GADTs
data Steps s a r where
    -- These constructors describe the tree of values, as above
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    -- Doc: The process that returns the value of type @a@ which is followed by a parser returning a value of type @b@.
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    -- Doc: Takes a process that returns a function @f@ of type @b -> a@ and is
    -- followed by a process returning a value @x@ of type @b@.  The resulting
    -- process will return the result of applying the function @f@ to @x@.
    Stop  ::                                   Steps s Void Void
 
    -- These constructors describe the parser state
    Shift ::             Steps s a r        -> Steps s a r
    Done  ::             Steps s a r        -> Steps s a r
    -- Doc: The parser that signals success.  The argument is the continuation.
    Fails ::                                   Steps s a r
    -- Doc: The parser that signals failure.
    Dislike :: Steps s a r ->                                   Steps s a r

    Suspend :: ([s] -> Steps s a r) -> Steps s a r
    -- Doc: A suspension of the parser (this is the part borrowed from
    -- Parallel Parsing Processes) The parameter to suspend's
    -- continuation is a whole chunk of text; [] represents the
    -- end of the input
   
    Best :: Ordering -> Profile -> Steps s a r -> Steps s a r -> Steps s a r

-- profile !! s = number of Dislikes found to do s Shifts
data Profile = PSusp | PFail | PRes Int | !Int :> Profile
    deriving Show

mapSucc PSusp = PSusp
mapSucc PFail = PFail
mapSucc (PRes x) = PRes (succ x) 
mapSucc (x :> xs) = succ x :> mapSucc xs

-- Map lookahead to maximum dislike difference we accept. When looking much further,
-- we are more prone to discard smaller differences. It's essential that this drops to zero when
-- its argument increases, so that we can discard things with dislikes using only
-- finite lookahead.
dislikeThreshold :: Int -> Int
-- dislikeThreshold n | n < 2 = 1
dislikeThreshold n = 0

-- | Compute the combination of two profiles, as well as which one is the best.
better :: Int -> Profile -> Profile -> (Ordering, Profile)
better _ PFail p = (GT, p) -- avoid failure
better _ p PFail = (LT, p)
better _ PSusp _ = (EQ, PSusp) -- could not decide before suspension => leave undecided.
better _ _ PSusp = (EQ, PSusp)
better _ (PRes x) (PRes y) = if x <= y then (LT, PRes x) else (GT, PRes y)  -- two results, just pick the best.
better lk xs@(PRes x) (y:>ys) = if x == 0 || y-x > dislikeThreshold lk then (LT, xs) else min x y +> better (lk+1) xs ys
better lk (y:>ys) xs@(PRes x) = if x == 0 || y-x > dislikeThreshold lk then (GT, xs) else min x y +> better (lk+1) ys xs
better lk (x:>xs) (y:>ys)
    | x == 0 && y == 0 = rec -- never drop things with no error: this ensures to find a correct parse if it exists.
    | y - x > threshold = (LT, x:>xs) -- if at any point something is too disliked, drop it.
    | x - y > threshold = (GT, y:>ys)
    | otherwise = rec
    where threshold = dislikeThreshold lk
          rec = min x y +> better (lk + 1) xs ys

x +> ~(ordering, xs) = (ordering, x :> xs)

profile :: Steps s a r -> Profile
profile (Val _ p) = profile p
profile (App p) = profile p
profile (Stop) = error "profile: Stop" -- this should always be "hidden" by Done
profile (Shift p) = 0 :> profile p
profile (Done _) = PRes 0 -- success with zero dislikes
profile (Fails) = PFail
profile (Dislike p) = mapSucc (profile p)
profile (Suspend _) = PSusp
profile (Best _ pr _ _) = pr


instance Show (Steps s a r) where
    show (Val _ p) = "v" ++ show p
    show (App p) = "*" ++ show p
    show (Stop) = "1"
    show (Shift p) = ">" ++ show p
    show (Done p) = "!" ++ show p
    show (Dislike p) = "?" ++ show p
    show (Fails) = "0"
    show (Suspend _) = "..."
    show (Best _ _ p q) = "(" ++ show p ++ ")" ++ show q


-- | Right-eval a fully defined process (ie. one that has no Suspend)
-- Returns value and continuation.
evalR :: Steps s a r -> (a, r)
evalR z@(Val a r) = (a,r)
evalR (App s) = let (f, s') = evalR s
                    (x, s'') = evalR s'
                in (f x, s'')
evalR Stop = error "evalR: Can't create values of type Void"
evalR (Shift v) = evalR v
evalR (Done v)  = evalR v
evalR (Dislike v) = evalR v
evalR (Fails) = error "evalR: No parse!"
evalR (Suspend _) = error "evalR: Not fully evaluated!"
evalR (Best choice _ p q) = case choice of
    LT -> evalR p
    GT -> evalR q
    EQ -> error $ "evalR: Ambiguous parse: " ++ show p ++ " ~~~ " ++ show q

-- | Pre-compute a left-prefix of some steps (as far as possible)
evalL :: Steps s a r -> Steps s a r
evalL (Shift p) = evalL p
evalL (Dislike p) = evalL p
evalL (Val x r) = Val x (evalL r)
evalL (App f) = case evalL f of
                  (Val a (Val b r)) -> Val (a b) r
                  (Val f1 (App (Val f2 r))) -> App (Val (f1 . f2) r)
                  r -> App r
evalL x@(Best choice _ p q) = case choice of
    LT -> evalL p
    GT -> evalL q
    EQ -> x -- don't know where to go: don't speculate on evaluating either branch.
evalL x = x

-- | Intelligent, caching best.
iBest p q = let ~(choice, pr) = better 0 (profile p) (profile q) in Best choice pr p q

-- | Push a chunk of symbols or eof in the process. This forces some suspensions.
push :: Maybe [s] -> Steps s a r -> Steps s a r
push (Just []) p = p  -- nothing more left to push
push ss p = case p of
                  (Suspend f) -> case ss of
                      Nothing -> f []
                      Just ss' -> f ss'
                  (Dislike p') -> Dislike (push ss p')
                  (Shift p') -> Shift (push ss p')
                  (Done p') -> Done (push ss p')
                  (Val x p') -> Val x (push ss p')
                  (App p') -> App (push ss p')
                  Stop -> Stop
                  Fails -> Fails
                  Best _ _ p' q' -> iBest (push ss p') (push ss q')
                  -- TODO: it would be nice to be able to reuse the profile here.

repush :: [s] -> Steps s a r -> Steps s a r
repush [] = pushEof
repush input = pushSyms input

-- | Push some symbols.
pushSyms :: [s] -> Steps s a r -> Steps s a r
pushSyms x = push (Just x)

-- | Push eof
pushEof :: Steps s a r -> Steps s a r
pushEof = push Nothing

-- | A parser. (This is actually a parsing process segment)
newtype P s a = P (forall b r. Steps s b r -> Steps s a (Steps s b r))

instance Functor (P s) where
    fmap f x = pure f <*> x

instance Applicative (P s) where
    P f <*> P x = P (App . f . x)
    pure x = P (Val x)

instance Alternative (P s) where
    empty = P $ \_fut -> Fails
    P a <|> P b = P $ \fut -> iBest (a fut) (b fut)

runP :: forall s a. P s a -> Process s a
runP (P p) = p (Done Stop)

-- | Run a parser.
runPolish :: forall s a. P s a -> [s] -> a
runPolish p input = fst $ evalR $ pushEof $ pushSyms input $ runP p

-- | Parse a symbol
symbol :: (s -> Bool) -> P s s
symbol f = P $ \fut -> Suspend $ \input ->
              case input of
                [] -> Fails -- This is the eof!
                (s:ss) -> if f s then Shift (Val s (push (Just ss) (fut)))
                                 else Fails

lookNext :: (Maybe s -> Bool) -> P s ()
lookNext f = P $ \fut -> Suspend $ \input ->
   if (f $ listToMaybe input) then Val () (repush input fut)
                              else Fails
        

-- | Parse the eof
eof :: P s ()
eof = P $ \fut -> Suspend $ \input ->
              case input of
                [] -> Shift (Val () (push Nothing fut))
                _ -> Fails

-- | Parse the same thing as the argument, but will be used only as
-- backup. ie, it will be used only if disjuncted with a failing
-- parser.
recoverWith :: forall s a. P s a -> P s a
recoverWith (P p) = P (Dislike . p)


----------------------------------------------------

type State st token result = (st, Process token result)

scanner :: forall st token result. P token result -> Scanner st token -> Scanner (State st token result) result
scanner parser input = Scanner 
    {
      scanInit = (scanInit input, runP parser),
      scanLooked = scanLooked input . fst,
      scanRun = run,
      scanEmpty = fst $ evalR $ pushEof $ runP parser
    }
    where
        run :: State st token result -> [(State st token result, result)]
        run (st,process) = updateState0 process $ scanRun input st

        updateState0 :: Process token result -> [(st,token)] -> [(State st token result, result)]
        updateState0 _        [] = []
        updateState0 curState toks@((st,tok):rest) = ((st, curState), result) : updateState0 nextState rest
            where nextState =       evalL $           pushSyms [tok]           $ curState
                  result    = fst $ evalR $ pushEof $ pushSyms (fmap snd toks) $ curState


------------------

data Expr = V Int | Add Expr Expr
            deriving Show

pExprParen = symbol (== '(') *> pExprTop <* symbol (== ')')

pExprVal = V <$> toInt <$> symbol (isDigit)
    where toInt c = ord c - ord '0'

pExprAtom = pExprVal <|> pExprParen

pExprAdd = pExprAtom <|> Add <$> pExprAtom <*> (symbol (== '+') *> pExprAdd) 

pExprTop = pExprAdd

pExpr = pExprTop <* eof

