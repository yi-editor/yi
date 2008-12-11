-- Copyright (c) JP Bernardy 2008
{-# OPTIONS -fglasgow-exts #-}
module Yi.IncrementalParse (Process, 
                            recoverWith, symbol, eof, lookNext, testNext, runPolish, 
                            runP, profile, pushSyms, pushEof, 
                            P, AlexState (..), scanner) where
import Yi.Lexer.Alex (AlexState (..))
import Yi.Prelude
import Prelude (Ordering(..))
import Yi.Syntax
import Data.List hiding (map, minimumBy)

data a :< b = (:<) {top :: a, _rest :: b}
infixr :<

type P s a = Parser s a

-- | Parser specification
data Parser s a where
    Pure :: a -> Parser s a
    Appl :: Parser s (b -> a) -> Parser s b -> Parser s a

    Bind :: Parser s a -> (a -> Parser s b) -> Parser s b

    Look :: Parser s a -> (s -> Parser s a) -> Parser s a
    Shif :: Parser s a -> Parser s a
    Empt :: Parser s a
    Disj :: Parser s a -> Parser s a -> Parser s a
    Yuck :: Parser s a -> Parser s a
    

-- | Parser process
data Steps s a where
    Val   :: a -> Steps s r                      -> Steps s (a :< r)
    App   :: Steps s ((b -> a) :< (b :< r))      -> Steps s (a :< r)
    Done  ::                               Steps s ()
    Shift ::           Steps s a        -> Steps s a
    Sh' ::             Steps s a        -> Steps s a
    Fail  ::                                Steps s a
    Sus    :: Steps s a -> (s -> Steps s a) -> Steps s a
    Best  :: Ordering -> Profile -> Steps s a -> Steps s a -> Steps s a
    Dislike :: Steps s a -> Steps s a


-- profile !! s = number of Dislikes found to do s Shifts
data Profile = PSusp | PFail | PRes Int | !Int :> Profile
    deriving Show

mapSucc :: Profile -> Profile
mapSucc PSusp = PSusp
mapSucc PFail = PFail
mapSucc (PRes x) = PRes (succ x) 
mapSucc (x :> xs) = succ x :> mapSucc xs

-- Map lookahead to maximum dislike difference we accept. When looking much further,
-- we are more prone to discard smaller differences. It's essential that this drops below 0 when
-- its argument increases, so that we can discard things with dislikes using only
-- finite lookahead.
dislikeThreshold :: Int -> Int
dislikeThreshold n 
    | n < 5 = 0
    | otherwise = -1 -- we looked 5 tokens ahead, and still have no clue who is the best. Pick at random.

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
    | x - y > threshold = (GT, y:>ys)
    | y - x > threshold = (LT, x:>xs) -- if at any point something is too disliked, drop it.
    | otherwise = rec
    where threshold = dislikeThreshold lk
          rec = min x y +> better (lk + 1) xs ys

(+>) :: Int -> (t, Profile) -> (t, Profile)
x +> ~(ordering, xs) = (ordering, x :> xs)

profile :: Steps s r -> Profile
profile (Val _ p) = profile p
profile (App p) = profile p
profile (Shift p) = 0 :> profile p
profile (Done) = PRes 0 -- success with zero dislikes
profile (Fail) = PFail
profile (Dislike p) = mapSucc (profile p)
profile (Sus _ _) = PSusp
profile (Best _ pr _ _) = pr
profile (Sh' _) = error "Sh' should be hidden by Sus"

instance Show (Steps s r) where
    show (Val _ p) = "v" ++ show p
    show (App p) = "*" ++ show p
    show (Done) = "1"
    show (Shift p) = ">" ++ show p
    show (Sh' p) = "'" ++ show p
    show (Dislike p) = "?" ++ show p
    show (Fail) = "0"
    show (Sus _ _) = "..."
    show (Best _ _ p q) = "(" ++ show p ++ ")" ++ show q


apply :: forall t t1 a. ((t -> a) :< (t :< t1)) -> a :< t1
apply ~(f:< ~(a:<r)) = f a :< r

-- | Right-eval a fully defined process (ie. one that has no Sus)
evalR' :: Steps s r -> r
evalR' Done = ()
evalR' (Val a r) = a :< evalR' r
evalR' (App s) = apply (evalR' s)
evalR' (Shift v) = evalR' v
evalR' (Dislike v) = evalR' v
evalR' (Fail) = error "evalR: No parse!"
evalR' (Sus _ _) = error "evalR: Not fully evaluated!"
evalR' (Sh' _) = error "evalR: Sh' should be hidden by Sus"
evalR' (Best choice _ p q) = case choice of
    LT -> evalR' p
    GT -> evalR' q
    EQ -> error $ "evalR: Ambiguous parse: " ++ show p ++ " ~~~ " ++ show q


instance Functor (Parser s) where
    fmap f = (pure f <*>)

instance Applicative (Parser s) where
    (<*>) = Appl
    pure = Pure

instance Alternative (Parser s) where
    (<|>) = Disj
    empty = Empt

instance Monad (Parser s) where
    (>>=) = Bind
    return = pure

toQ :: Parser s a -> forall h r. ((h,a) -> Steps s r)  -> (h -> Steps s r)
toQ (Look a f) = \k h -> Sus (toQ a k h) (\s -> toQ (f s) k h)
toQ (p `Appl` q) = \k -> toQ p $ toQ q $ \((h, b2a), b) -> k (h, b2a b)
toQ (Pure a)     = \k h -> k (h, a)
toQ (Disj p q)   = \k h -> iBest (toQ p k h) (toQ q k h)
toQ (Bind p a2q) = \k -> (toQ p) (\(h,a) -> toQ (a2q a) k h)
toQ Empt = \_k _h -> Fail
toQ (Yuck p) = \k h -> Dislike $ toQ p k h
toQ (Shif p) = \k h -> Sh' $ toQ p k h

toP :: Parser s a -> forall r. (Steps s r)  -> (Steps s (a :< r))
toP (Look a f) = \fut -> Sus (toP a fut) (\s -> toP (f s) fut)
toP (Appl f x) = App . toP f . toP x
toP (Pure x)   = Val x
toP Empt = \_fut -> Fail
toP (Disj a b)  = \fut -> iBest (toP a fut) (toP b fut)
toP (Bind p a2q) = \fut -> (toQ p) (\(_,a) -> (toP (a2q a)) fut) ()
toP (Yuck p) = Dislike . toP p 
toP (Shif p) = Sh' . toP p

-- | Intelligent, caching best.
iBest :: Steps s a -> Steps s a -> Steps s a
iBest p q = let ~(choice, pr) = better 0 (profile p) (profile q) in Best choice pr p q

symbol :: forall s. (s -> Bool) -> Parser s s
symbol f = Look empty $ \s -> if f s then (Shif $ pure s) else empty

eof :: forall s. Parser s ()
eof = Look (pure ()) (const empty)

-- | Push a chunk of symbols or eof in the process. This forces some suspensions.
feed :: Maybe [s] -> Steps s r -> Steps s r
feed (Just []) p = p  -- nothing more left to feed
feed ss p = case p of
                  (Sus nil cons) -> case ss of
                      Just [] -> p -- no more info, stop feeding
                      Nothing -> feed Nothing nil -- finish
                      Just (s:_) -> feed ss (cons s)
                  (Shift p') -> Shift (feed ss p')
                  (Sh' p')   -> Shift (feed (fmap (drop 1) ss) p')
                  (Dislike p') -> Dislike (feed ss p')
                  (Val x p') -> Val x (feed ss p')
                  (App p') -> App (feed ss p')
                  Done -> Done
                  Fail -> Fail
                  Best _ _ p' q' -> iBest (feed ss p') (feed ss q')
                  -- TODO: it would be nice to be able to reuse the profile here.


feedZ :: Maybe [s] -> Zip s r -> Zip s r
feedZ x = onRight (feed x)


evalZL :: forall s a. Zip s a -> Zip s a
evalZL = onLeft simplify . simplAll
    where simplAll = right simplAll . onLeft simplify


-- | Push some symbols.
pushSyms :: forall s r. [s] -> Zip s r -> Zip s r
pushSyms x = feedZ (Just x)

-- | Push eof
pushEof :: forall s r. Zip s r -> Zip s r
pushEof = feedZ Nothing

runP :: forall s a. P s a -> Process s a
runP p = Zip RStop (toP p Done)

-- | Run a parser.
runPolish :: forall s a. P s a -> [s] -> a
runPolish p input = top $ evalZR $ pushEof $ pushSyms input $ runP p

testNext :: (Maybe s -> Bool) -> P s ()
testNext f = Look (if f Nothing then ok else empty) (\s -> 
   if (f $ Just s) then ok else empty)
    where ok = pure ()

lookNext :: P s (Maybe s)
lookNext = Look (pure Nothing) (\s -> pure (Just s))

        

-- | Parse the same thing as the argument, but will be used only as
-- backup. ie, it will be used only if disjuncted with a failing
-- parser.
recoverWith :: forall s a. P s a -> P s a
recoverWith = Yuck

----------------------------------------------------

--------------------------------
-- The zipper for efficient evaluation:

-- Arbitrary expressions in Reverse Polish notation.
-- This can also be seen as an automaton that transforms a stack.
-- RPolish is indexed by the types in the stack consumed by the automaton (input),
-- and the stack produced (output)
data RPolish input output where
  RPush :: a -> RPolish (a :< rest) output -> RPolish rest output
  RApp :: RPolish (b :< rest) output -> RPolish ((a -> b) :< a :< rest) output 
  RStop :: RPolish rest rest

-- Evaluate the output of an RP automaton, given an input stack
evalRP :: RPolish input output -> input -> output
evalRP RStop acc = acc 
evalRP (RPush v r) acc = evalRP r (v :< acc)
evalRP (RApp r) (f :< a :< acc) = evalRP r (f a :< acc)


-- execute the automaton as far as possible
simplify :: RPolish s output -> RPolish s output
simplify (RPush a (RPush f (RApp r))) = simplify (RPush (f a) r)
simplify x = x

evalZR :: Zip token output -> output
evalZR (Zip l r) = evalRP l (evalR' r)

-- Gluing a Polish expression and an RP automaton.
-- This can also be seen as a zipper of Polish expressions.
data Zip s output where
   Zip :: RPolish mid output -> Steps s mid -> Zip s output
   -- note that the Stack produced by the Polish expression matches
   -- the stack consumed by the RP automaton.

-- Move the zipper to right, and call continuation if something was pushed in
-- the left part.
right :: (Zip s output -> Zip s output) -> (Zip s output -> Zip s output) 
right k x@(Zip l rhs) = case rhs of
    (Val a r) -> k (Zip (RPush a l) r)
    (App r)  -> k (Zip (RApp l) r)
    (Shift p) -> right k (Zip l p)
    (Dislike p) -> right k (Zip l p)
    (Best choice _ p q) -> case choice of
        LT -> right k (Zip l p)
        GT -> right k (Zip l q)
        EQ -> x -- don't know where to go: don't speculate on evaluating either branch.
    _ -> x


onLeft :: (forall i o. RPolish i o -> RPolish i o) -> Zip s a -> Zip s a
onLeft f (Zip x y) = (Zip (f x) y)

onRight :: (forall r. Steps s r -> Steps s r) -> Zip s a -> Zip s a
onRight f (Zip x y) = Zip x (f y)


----------------------------------------------------
type Process token result = Zip token (result :< ())
type State st token result = (st, Process token result)

scanner :: forall st token result. P token result -> Scanner st token -> Scanner (State st token result) result
scanner parser input = Scanner 
    {
      scanInit = (scanInit input, runP parser),
      scanLooked = scanLooked input . fst,
      scanRun = run,
      scanEmpty = top $ evalZR $ pushEof $ runP parser
    }
    where
        run :: State st token result -> [(State st token result, result)]
        run (st,process) = updateState0 process $ scanRun input st

        updateState0 :: Process token result -> [(st,token)] -> [(State st token result, result)]
        updateState0 _        [] = []
        updateState0 curState toks@((st,tok):rest) = ((st, curState), result) : updateState0 nextState rest
            where nextState =       evalZL $           pushSyms [tok]           $ curState
                  result    = top $ evalZR $ pushEof $ pushSyms (fmap snd toks) $ curState

