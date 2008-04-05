{-# OPTIONS -fglasgow-exts #-}
module Yi.IncrementalParse (Process, Void, 
                            symbol, eof, runPolish, run, 
                            P, AlexState (..), mkHighlighter) where
import Yi.Syntax.Alex (AlexState (..))
import Control.Applicative
import Yi.Prelude
import Prelude ()
import Yi.Syntax
import Data.List hiding (map)

data Void

data Steps s a r where
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    Stop  ::                                   Steps s Void Void
    Shift :: (Maybe s -> Steps s a r)       -> Steps s a r -- TODO: add default sym.
    Done  ::             Steps s a r        -> Steps s a r
    Fails ::                                   Steps s a r

-- data F a b where
--     Snoc :: F a b -> (b -> c) -> F a c
--     Nil  :: F a b
-- 
-- data S s a r where
--     S :: F a b -> Steps s a r -> S s a r


-- | Right-eval a fully defined process (ie. one that has no Shift/Done/Fails)
evalR :: Steps s a r -> (a, r)
evalR (Val a r) = (a,r)
evalR (App s) = let (f, s') = evalR s
                    (x, s'') = evalR s'
                in (f x, s'')
evalR Stop = error "Can't create values of type Void"



-- | Pre-compute a left-prefix of some steps (as far as possible)
evalL :: Steps s a r -> Steps s a r
evalL (Val x r) = Val x (evalL r)
evalL (App f) = case evalL f of
                  (Val a (Val b r)) -> Val (a b) r
                  (Val f1 (App (Val f2 r))) -> App (Val (f1 . f2) r)
                  r -> App r
evalL x = x


-- | Push a some symbols in the process
pushSyms :: [s] -> Steps s a r -> Steps s a r
pushSyms ss p = case p of
                  (Shift f) -> case ss of 
                          (s:ss') -> pushSyms ss' (f $ Just s)
                          [] -> p
                  (Done p') -> Done (pushSyms ss p')
                  Fails -> error "pushSyms: parser fails!"
                  (Val x p') -> Val x (pushSyms ss p')
                  (App p') -> App (pushSyms ss p')
                  Stop -> Stop

-- | Get rid of all input-related constructors by pushing the EOF through the whole process
pushEof :: Steps s a r -> Steps s a r
pushEof p = case p of
                  (Val x p') -> Val x (pushEof p')
                  (App p') -> App (pushEof p')
                  Stop -> Stop
                  (Shift f) -> pushEof (f Nothing)
                  Done p' -> pushEof p'
                  Fails -> error "pushEof: parser fails!"


newtype P s a = P (forall b r. Steps s b r -> Steps s a (Steps s b r))

instance Functor (P s) where
    fmap f x = pure f <*> x

instance Applicative (P s) where
    P f <*> P x = P (App . f . x)
    pure x = P (Val x)

instance Alternative (P s) where
    empty = P $ \_fut -> Fails
    P a <|> P b = P $ \fut -> best (a fut) (b fut)


-- | Get the process' value (online).  Returns value, continuation and
--   remaining input.
evalSteps :: Steps s a (Steps s b r) -> [(st,s)] -> (a, Steps s b r, [(st,s)])
evalSteps (Val a s) xs = (a, s, xs)
evalSteps (Shift v) xs = let (s, xs') = front xs
                         in evalSteps (v $ fmap snd s) xs'
evalSteps (Done v)  xs = evalSteps v xs
evalSteps (Fails)   xs = (error "evalSteps: no parse", Fails, xs)
evalSteps (App s)   xs = let (f,s',  xs')  = evalSteps s  xs
                             (a,s'', xs'') = evalSteps s' xs'
                         in (f a, s'', xs'')

front [] = (Nothing, [])
front (x:xs) = (Just x, xs)



-- | Advance in the result steps, pushing results in the continuation.
-- (Must return one of: Done, Shift, Fail)
getProgress :: (Steps s a r -> Steps s b t) -> Steps s a r -> Steps s b t
getProgress f (Val a s) = getProgress (f . Val a) s
getProgress f (App s)   = getProgress (f . App) s
-- getProgress f Stop   = f Stop
getProgress f (Done p)  = Done (f p)
getProgress f (Shift s) = Shift (\input -> f (s input))
getProgress f Fails    = Fails

best :: Steps x a s -> Steps x a s ->  Steps x a s
--l `best` r | trace ("best: "++show (l,r)) False = undefined
Fails   `best` Fails   = Fails
Fails   `best` p       = p
q       `best` Fails   = q
Done _  `best` Done _  = error "ambiguous grammar"
Done a  `best` q       = Done a
p       `best` Done a  = Done a
Shift v `best` Shift w = Shift (\input -> v input `best` w input)
p       `best` q       = getProgress id p `best` getProgress id q


shift :: P s (Maybe s)
shift = P $ \fut -> Shift (\s -> Val s fut)

run (P p) = p (Done Stop)

type Process s a = Steps s a (Steps s Void Void)

runPolish p = fst3 . evalSteps (run p)

symbol f = P $ \fut -> Shift $ \input -> 
                                    case input of
                                      Just s -> if f s
                                                  then Val s fut
                                                  else Fails
                                      Nothing -> Fails

eof = P $ \fut -> Shift $ \input -> 
                                    case input of
                                      Just _ -> Fails
                                      Nothing -> Val () fut


check f p = p <*> (\x -> if f x then pure x else empty)

----------------------------------------------------

type States st token result = [(st, Process token result)]
data Cache st token result = Cache result (States st token result)

splitBy n [] = []
splitBy n l = let (x,y) = splitAt n l in x : splitBy n y


mkHighlighter :: forall lexState token result st.
                 P token result
              -> (Int -> Int -> result -> [Stroke])
              -> (st -> AlexState lexState)
              -> Highlighter st token (Cache st token result)
mkHighlighter parser getStrokes getAlexState = 
  Yi.Syntax.SynHL { hlStartState   = Cache emptyResult []
                  , hlRun          = updateCache
                  , hlGetStrokes   = getStrokes'
                  }
      where
        emptyResult :: result
        emptyResult = fst $ evalR $ pushEof $ run parser

        getStrokes' :: Int -> Int -> Cache st token result -> [Stroke]
        getStrokes' start end (Cache r _) = getStrokes start end r

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
                  startState = (scanInit scanner, run parser)

        updateState0 :: Process token result -> [(st,token)] -> States st token result
        updateState0 _        [] = []
        updateState0 curState (toks:rest) = (fst $ toks, curState) : updateState0 nextState rest
            where nextState = evalL $ pushSyms [snd toks] $ curState


        -- FIXME: looked offset is wrong here. (due to splitBy)
        updateState :: Process token result -> [[(st,token)]] -> States st token result
        updateState _        [] = []
        updateState curState (toks:rest) = (fst $ head $ toks, curState) : updateState nextState rest
            where nextState = evalL $ pushSyms (fmap snd toks) $ curState
