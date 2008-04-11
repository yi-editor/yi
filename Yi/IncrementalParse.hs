-- Copyright (c) JP Bernardy 2008
{-# OPTIONS -fglasgow-exts #-}
module Yi.IncrementalParse (Process, Void, 
                            recoverWith, symbol, eof, runPolish,
                            P, AlexState (..), mkHighlighter) where
import Yi.Syntax.Alex (AlexState (..))
import Control.Applicative
import Yi.Prelude
import Prelude ()
import Yi.Syntax
import Data.List hiding (map)

data Void

type Process s a = Steps s a (Steps s Void Void)

data Steps s a r where
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    Stop  ::                                   Steps s Void Void
    Shift ::             Steps s a r        -> Steps s a r
    Done  ::             Steps s a r        -> Steps s a r
    Fails ::                                   Steps s a r
    Dislike :: Steps s a r ->                                   Steps s a r
    Suspend :: (Maybe [s] -> Steps s a r) -> Steps s a r

instance Show (Steps s a r) where
    show (Val _ p) = "v" ++ show p
    show (App p) = "*" ++ show p
    show (Stop) = "1"
    show (Shift p) = ">" ++ show p
    show (Done p) = "!" ++ show p
    show (Dislike p) = "?" ++ show p
    show (Fails) = "0"
    show (Suspend _) = "..."

-- data F a b where
--     Snoc :: F a b -> (b -> c) -> F a c
--     Nil  :: F a b
-- 
-- data S s a r where
--     S :: F a b -> Steps s a r -> S s a r


-- | Right-eval a fully defined process (ie. one that has no Suspend)
-- Returns value and continuation.
evalR :: Steps s a r -> (a, r)
evalR (Val a r) = (a,r)
evalR (App s) = let (f, s') = evalR s
                    (x, s'') = evalR s'
                in (f x, s'')
evalR Stop = error "evalR: Can't create values of type Void"
evalR (Shift v) = evalR v
evalR (Done v)  = evalR v
evalR (Dislike v) = -- trace "Yuck!" $ 
                    evalR v
evalR (Fails) = error "evalR: No parse!"
evalR (Suspend _) = error "evalR: Not fully evaluated!"


-- | Pre-compute a left-prefix of some steps (as far as possible)
evalL :: Steps s a r -> Steps s a r
evalL (Shift p) = evalL p
evalL (Dislike p) = evalL p
evalL (Val x r) = Val x (evalL r)
evalL (App f) = case evalL f of
                  (Val a (Val b r)) -> Val (a b) r
                  (Val f1 (App (Val f2 r))) -> App (Val (f1 . f2) r)
                  r -> App r
evalL x = x


-- | Push some symbols in the process
push :: Maybe [s] -> Steps s a r -> Steps s a r
push ss p = case p of
                  (Suspend f) -> f ss
                  (Dislike p') -> Dislike (push ss p')
                  (Shift p') -> Shift (push ss p')
                  (Val x p') -> Val x (push ss p')
                  (App p') -> App (push ss p')
                  Stop -> Stop
                  Fails -> Fails

pushSyms :: [s] -> Steps s a r -> Steps s a r
pushSyms x = push (Just x)

pushEof :: Steps s a r -> Steps s a r
pushEof = push Nothing

newtype P s a = P (forall b r. Steps s b r -> Steps s a (Steps s b r))

instance Functor (P s) where
    fmap f x = pure f <*> x

instance Applicative (P s) where
    P f <*> P x = P (App . f . x)
    pure x = P (Val x)

instance Alternative (P s) where
    empty = P $ \_fut -> Fails
    P a <|> P b = P $ \fut -> best (a fut) (b fut)



-- | Advance in the result steps, pushing results in the continuation.
-- (Must return one of: Done, Shift, Fail)
getProgress :: (Steps s a r -> Steps s b t) -> Steps s a r -> Steps s b t
getProgress f (Val a s) = getProgress (f . Val a) s
getProgress f (App s)   = getProgress (f . App) s
-- getProgress f Stop   = f Stop
getProgress f (Done p)  = Done (f p)
getProgress f (Shift p) = Shift (f p)
getProgress f (Dislike p) = Dislike (f p)
getProgress _ (Fails) = Fails
getProgress f (Suspend p) = Suspend (\input -> f (p input))



best :: Steps x a s -> Steps x a s ->  Steps x a s
--l `best` r | trace ("best: "++show (l,r)) False = undefined
Suspend f `best` Suspend g = Suspend (\input -> f input `best` g input)

Fails   `best` p       = p
p `best` Fails         = p

Dislike a `best` b = bestD a b
a `best` Dislike b = bestD b a

Done a  `best` Done _  = Done a -- error "ambiguous grammar"
                                -- There are sometimes many ways to fix an error. Pick the 1st one.
Done a  `best` _       = Done a
_       `best` Done a  = Done a

Shift v `best` Shift w = Shift (v `best` w)

p       `best` q       = getProgress id p `best` getProgress id q


-- as best, but lhs is disliked.
bestD :: Steps x a s -> Steps x a s ->  Steps x a s

Suspend f `bestD` Suspend g = Suspend (\input -> f input `bestD` g input)

Fails   `bestD` p       = p
p `bestD` Fails         = Dislike p

a `bestD` Dislike b = Dislike (best a b)  -- back to equilibrium (prefer to do this, hence 1st case)
Dislike _ `bestD` b = b -- disliked twice: forget it.

Done _  `bestD` Done a  = Done a -- we prefer rhs in this case
Done a  `bestD` _       = Dislike (Done a)
_       `bestD` Done a  = Done a

Shift v `bestD` Shift w = Shift (v `bestD` w)
_       `bestD` Shift w = Shift w -- prefer shifting than keeping a disliked possibility forever


p       `bestD` q       = getProgress id p `bestD` getProgress id q



runP :: forall t t1.
                                    P t t1 -> Steps t t1 (Steps t Void Void)
runP (P p) = p (Done Stop)

runPolish :: forall s a. P s a -> [s] -> a
runPolish p input = fst $ evalR $ pushEof $ pushSyms input $ runP p

symbol :: (s -> Bool) -> P s s
symbol f = P (\fut -> Suspend (symHelper fut))
    where symHelper fut input = 
              case input of
                Nothing -> Fails -- This is the eof!
                Just [] ->  Suspend (symHelper fut) -- end of the chunk: to be continued
                Just (s:ss) -> if f s then push (Just ss) (Shift (Val s (fut)))
                               else Fails


eof :: P s ()
eof = P (\fut -> Suspend (symHelper fut))
    where symHelper fut input = 
              case input of
                Nothing -> Val () fut
                Just [] ->  Suspend (symHelper fut) -- end of the chunk: to be continued
                Just (_:_) -> Fails

recoverWith :: forall s a. P s a -> P s a
recoverWith (P p) = P (Dislike . p)


----------------------------------------------------

type States st token result = [(st, Process token result)]
data Cache st token result = Cache result (States st token result)


mkHighlighter :: forall lexState token result st.
                 P token result
              -> (Int -> Int -> Int -> result -> [Stroke])
              -> (st -> AlexState lexState)
              -> Highlighter st token (Cache st token result) result
mkHighlighter parser getStrokes getAlexState = 
  Yi.Syntax.SynHL { hlStartState   = Cache emptyResult []
                  , hlRun          = updateCache
                  , hlGetStrokes   = getStrokes'
                  , hlGetTree      = \(Cache result _) -> result
                  }
      where
        emptyResult :: result
        emptyResult = fst $ evalR $ pushEof $ runP parser

        getStrokes' :: Int -> Int -> Int -> Cache st token result -> [Stroke]
        getStrokes' point start end (Cache r _) = getStrokes point start end r

        updateCache :: Scanner st token
                     -> Int
                     -> Cache st token result
                     -> Cache st token result
        updateCache scanner dirtyOffset (Cache _ cachedStates) = Cache newResult newCachedStates

            where reused = takeWhile ((< dirtyOffset) . lookedOffset . getAlexState . fst) cachedStates
                  resumeState = if null reused then startState else last reused
                  newCachedStates = reused ++ recomputed
                  recomputed = updateState0 (snd resumeState) $ {-splitBy 20 -} text
                  text = scanRun scanner (fst resumeState)
                  newResult = -- trace ("restart: " ++ show (getAlexState (fst resumeState))) $ 
                      fst $ evalR $ pushEof $ pushSyms (fmap snd text) $ snd $ resumeState

                  startState :: (st, Process token result)
                  startState = (scanInit scanner, runP parser)

        updateState0 :: Process token result -> [(st,token)] -> States st token result
        updateState0 _        [] = []
        updateState0 curState (toks:rest) = (fst $ toks, curState) : updateState0 nextState rest
            where nextState = evalL $ pushSyms [snd toks] $ curState


        -- FIXME: looked offset is wrong here. (due to splitBy)
        updateState :: Process token result -> [[(st,token)]] -> States st token result
        updateState _        [] = []
        updateState curState (toks:rest) = (fst $ head $ toks, curState) : updateState nextState rest
            where nextState = evalL $ pushSyms (fmap snd toks) $ curState
