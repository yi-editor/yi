{-# OPTIONS -fglasgow-exts #-}
module Yi.IncrementalParse (IResult, Process, Result(..), Void, upd, symbol, eof, runPolish, run, getValue) where

import Control.Applicative

data Void

data Steps s a r where
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    Stop  ::                                   Steps s Void Void
    Shift :: (Maybe s -> Steps s a r)       -> Steps s a r -- TODO: add default sym.
    Done  ::             Steps s a r        -> Steps s a r
    Fails ::                                   Steps s a r

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
                  r -> App r
evalL x = x


-- | Push a number of symbols in the process
pushSyms :: [s] -> Steps s a r -> Steps s a r
pushSyms ss p = case p of
                  (Shift f) -> case ss of 
                          (s:ss') -> pushSyms ss' (f $ Just s)
                          [] -> p
                  (Val x p') -> Val x (pushSyms ss p')
                  (App p') -> App (pushSyms ss p')
                  Stop -> Stop

-- | Get rid of all shifts by pushing the EOF through the whole process
pushEof :: Steps s a r -> Steps s a r
pushEof p = case p of
                  (Shift f) -> pushEof (f Nothing)
                  (Val x p') -> Val x (pushEof p')
                  (App p') -> App (pushEof p')
                  Stop -> Stop


newtype P s a = P (forall b r. Steps s b r -> Steps s a (Steps s b r))

instance Functor (P s) where
    fmap f x = pure f <*> x

instance Applicative (P s) where
    P f <*> P x = P (App . f . x)
    pure x = P (Val x)

instance Alternative (P s) where
    empty = P $ \fut -> Fails
    P a <|> P b = P $ \fut -> best (a fut) (b fut)


-- | Get the process' value (online).  Returns value, continuation and
--   remaining input.
evalSteps :: Steps s a (Steps s b r) -> [s] -> (a, Steps s b r, [s])
evalSteps (Val a s) xs = (a, s, xs)
evalSteps (Shift v) [] = evalSteps (v Nothing) []
evalSteps (Shift v) (x:xs) = evalSteps (v $ Just x) xs
evalSteps (Done v)  xs = evalSteps v xs
evalSteps (Fails)   xs = (error "evalSteps: no parse", Fails, xs)
evalSteps (App s)   xs = let (f,s',  xs')  = evalSteps s  xs
                             (a,s'', xs'') = evalSteps s' xs'
                         in (f a, s'', xs'')

data Result s a r where
    Leaf :: a -> !Int -> Steps s a r -> Result s a r
    Bin  :: a 
              -> !Int
              -> (Result s (b->a) (Steps s b r)) -- left partial result
              -> Result s b r                    -- right partial result
              -> Result s a r

getValue (Leaf a _ _) = a
getValue (Bin a _ _ _) = a

getOfs (Leaf _ o _) = o
getOfs (Bin _ o _ _) = o


bin l r = Bin ((getValue l) (getValue r)) (getOfs l) l r

upd :: forall s a b r. (s -> Int) -> (Int -> [s]) -> Int -> Result s a (Steps s b r) -> (Result s a (Steps s b r), Steps s b r, [s])
upd tokOfs source dirty p = update p
    where 
      -- Invariant: ofs >= startOf p
      update :: forall a b r. Result s a (Steps s b r) 
             -> (Result s a (Steps s b r), Steps s b r, [s])
      update (Leaf _ o p) = evalResult p (source o)
      update (Bin a _ l r)
             | dirty < o   = let (l',s',xs') = update l
                                 (r',s'',xs'') = evalResult s' xs'
                             in (bin l' r', s'', xs'') 
             | otherwise = let (r',s'',xs'') = update r
                               in (bin l r', s'', xs'')
          where o = getOfs r

      evalResult :: forall a b r. Steps s a (Steps s b r) -> [s] 
                 -> (Result s a (Steps s b r), Steps s b r, [s])
      evalResult (App s) xs = let (f, s', xs' ) = evalResult s  xs
                                  (a, s'', xs'') = evalResult s' xs'
                              in (bin f a, s'', xs'')
      evalResult steps xs = let (a, s', xs') = evalSteps steps xs 
                            in (Leaf a (inpOfs xs) steps, s', xs')

      inpOfs [] = 0 -- Hack: we'll always recreate the right spine; that's
                    -- fine because it must always be recreated anyway.
      inpOfs (s:_) = tokOfs s

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

type IResult s a = Result s a (Steps s Void Void)

fst3 (x,_,_) = x


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
