{-# OPTIONS -fglasgow-exts #-}

module IncrementalParser2 where

import Data.Generics
import Data.Char
import Control.Applicative

{- ----------------------------------------

Incremental parser prototype, 2nd version.

Key points:

- The basis is a mix between "Polish Parsers, Step by Step (Hughes and Swierstra)", 
  and "Parallel Parsing Processes (Claessen)"
  
  (It's strongly advised to read the papers!)

  This lifts the LL(k) restriction of the previous implementation.


- The parser has "online" behaviour.

  This is a big advantage because we don't have to parse the whole file to begin syntax 
  highlight the beginning of it.

- Resilient to insert parse errors. 

  The structure around the inserted error
  is preserved. This would allow things like "wiggly underlining" of errors in realtime;
  also things like indentation, etc. can continue working at a coarse level in presence
  of errors.
  
- Based on Applicative functors.

  This is not as powerful as Monadic parsers, but easier to work with. This is needed if
  we want to build the result lazily.


-------------------------------------------}


-- | A parser, on the applicative functor model (see Swierstra papers)
data P s a where
    Symb :: (s -> String) -> String -> (s -> Bool) -> P s s
    Fail :: String -> P s a
    Pure :: a -> P s a
    Pipe :: P s a -> P s a -> P s a
    Star :: P s (a -> b) -> P s a -> P s b

instance Functor (P s) where
    fmap f p = pure f <*> p

instance Applicative (P s) where
    pure = Pure
    (<*>) = Star

instance Alternative (P s) where
    empty = Fail "no parse"
    (<|>) = Pipe

symbol :: Show s => String -> (s -> Bool) -> P s s
symbol s f = Symb show s f


-- | Interleaved parsing processes. This is a mix between 
-- "Polish Parsers, Step by Step (Hughes and Swierstra)", 
-- and "Parallel Parsing Processes (Claessen)"
data Steps s a r where
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    Shift :: ([s] -> Steps s a r)           -> Steps s a r
    Done  :: Steps s a r                    -> Steps s a r
    Fails :: String                         -> Steps s a r

-- For debugging:
instance Show (Steps x a s) where
    show (Val _ x) = "v" ++ show x
    show (App   x) = "." ++ show x
    show (Shift x) = ">" 
    show (Done  x) = "!" ++ show x
    show (Fails s) = "?" ++ s

-- | Choose the non-failing option, (or the one that fails latest)
best :: Steps x a s -> Steps x a s ->  Steps x a s
Fails x `best` Fails y = Fails (x ++ "|" ++ y) -- TODO: record list of expected stuff, etc. (see Parsek)
Fails _ `best` p       = p
q       `best` Fails _ = q
Done _  `best` Done _  = error "ambiguous grammar"
Done a  `best` q       = Done a
p       `best` Done a  = Done a
Shift v `best` Shift w = Shift (\input -> v input `best` w input)
p       `best` q       = getProgress id p `best` getProgress id q

-- | Advance in the result steps, pushing results in the continuation.
-- (Must return one of: Done, Shift, Fail)
getProgress :: (Steps s a r -> Steps s b t) -> Steps s a r -> Steps s b t
getProgress f (Val a s) = getProgress (f . Val a) s
getProgress f (App s)   = getProgress (f . App) s
getProgress f (Done p)  = Done (f p)
getProgress f (Shift s) = Shift (\input -> f (s input))
getProgress f (Fails s) = Fails s

-- | Get the process' value (non incrementally -- but still online!)
evalSteps :: Steps s a (Steps s b r) -> [s] -> (a, Steps s b r, [s])
evalSteps (Val a s) xs = (a, s, xs)
evalSteps (Shift v) xs = evalSteps (v xs) (drop 1 xs)
evalSteps (Done v)  xs = evalSteps v xs
evalSteps (Fails s) xs = (error s, Fails s, xs)
evalSteps (App s)   xs = let (f,s',  xs')  = evalSteps s  xs
                             (a,s'', xs'') = evalSteps s' xs'
                         in (f a, s'', xs'')


type Process s a b r = Steps s a (Steps s b r)

type ErrorMessage = String

-- | Parse tree, ready for incremental modification, repairing, etc.
data Result s a w r where
    Bin :: forall b b1. 
           Int                                 -> -- length of the thing (#syms)
           a                                   -> -- the value (cached)
           Process s a w r                     -> -- material to fully re-build.
           Bool                                -> -- contains some error node?
           Result s (b -> a) b (Steps s b1 r)  -> -- material to re-build left hand
           Result s b w r                      -> -- material to re-build right hand
           Result s a w r
    Tip :: a -> Process s a w r -> Result s a w r   -- An atomic value
    Err :: String -> a -> Process s a w r -> Result s a w r   -- An error
    Cha :: [s]  -> Bool -> Process s a w r -> Result s a w r -> Result s a w r   -- 

-- For debugging:
instance Show (Result Char a w r) where
    showsPrec d r = -- shows (getLength r) .
                    -- (if hasErr r then showChar '?' else showChar ' ') .
                    case r of 
                      (Tip _ _) -> id
                      (Cha s err _ p) -> (if err then  showChar '?' else id) . 
                                         showString s . showsPrec 11 p
                      (Err msg s _) -> showChar '!' . showString msg
                      (Bin n _a _p _err l r) -> -- showParen (d > 10) $ 
                                                showsPrec 11 l . showsPrec 11 r

getResult :: Result s a w r -> a
getResult (Bin _ x _ _ _ _) = x
getResult (Tip x _) = x
getResult (Err _ x _) = x
getResult (Cha _ _ _ x) = getResult x

getLength (Bin l _ _ _ _ _) = l
getLength (Tip _ _) = 0
getLength (Err _ _ _) = 0
getLength (Cha s _ _ r) = length s + getLength r

getParser (Bin _ _ p _ _ _) = p
getParser (Err _ _ p) = p
getParser (Tip _ p) = p
getParser (Cha _ _ p _) = p

getSymbols :: Result s a w r -> [s]
getSymbols (Bin _ _ _ _ l r) = getSymbols l ++ getSymbols r 
getSymbols (Tip _ _) = []
getSymbols (Err _ _ _) = []
getSymbols (Cha s _ _ r) = s ++ getSymbols r

hasErr (Bin _ _ _ err _ _) = err
hasErr (Err _ _ _) = True
hasErr (Tip _ _) = False
hasErr (Cha _ err _ r) = err || hasErr r
                     
-- | Get the process' "Result"
pEvalSteps :: Steps s a (Steps s b r) -> [s] -> (Result s a b r, Steps s b r, [s])
pEvalSteps p@(Val a s) xs = (Tip a p, s, xs)
pEvalSteps p@(Shift v) xs = let (proto, s', xs') = pEvalSteps (v xs) (drop 1 xs)
                            in (Cha (take 1 xs) False p proto, s', xs')
pEvalSteps p@(Done v)  xs = pEvalSteps v xs
pEvalSteps p@(Fails s) xs = (Err s (error s) p, Fails s, xs)
pEvalSteps p@(App s)   xs = (Bin (m + n) (f b) p (hasErr protoF || hasErr protoB) protoF protoB, s'', xs'')
    where (protoF,s',  xs')  = pEvalSteps s  xs
          (protoB,s'', xs'') = pEvalSteps s' xs'
          m = getLength protoF
          n = getLength protoB
          b = getResult protoB
          f = getResult protoF

fstrd (a,b,c) = (a,c)

-- | Repairs a Result that's been marked with errors
repair :: (Result s a b r, [s]) -> (Result s a b r, [s])
repair (Tip x p, over) = (Tip x p, over)
repair c@(Cha s err pr r, over) 
    | err = let (r', over') = fstrd (pEvalSteps pr (s ++ getSymbols r ++ over))
            in if hasErr r' then c else (r', over')
    | hasErr r = let (r', over') = repair (r,over)
                 in (Cha s err pr r', over')
    | otherwise = (Cha s err pr r, over) 
repair (Err _ _ p, over) = fstrd (pEvalSteps p over)
repair (r@(Bin l x p err rl rr), over)
    | not err    = ((Bin l  x  p err rl  rr),  over)
    -- when both sides have errors, we must re-sync at the common level.
    | hasErr rl && hasErr rr = fstrd $ pEvalSteps p (getSymbols r ++ over)
    -- if left is still in error, don't bother reparsing (we're as good with the current parse)
    | hasErr rl' = ((Bin l  x  p err  rl  rr),  over)
    | otherwise  = ((Bin l' x' p err' rl' rr'), over')
    where (rl', overl) = repair (rl, getSymbols rr ++ over)
          (rr', over') = if getLength rl == getLength rl' 
                         then repair (rr, over)
                         else fstrd $ pEvalSteps (getParser rr) overl
                         -- when the length of left is different, we cannot re-use the parser for right side.
          x' = (getResult rl') (getResult rr')
          l' = getLength rl' + getLength rr'
          err' = hasErr rl' || hasErr rr'


-- | Turns a parser specification into the corresponding parsing processes (in CPS)
mkPar :: P s a -> (forall b r.
                         (Steps s b r) ->
                         (Steps s a (Steps s b r)))
mkPar (Symb sho n f) = \k -> Shift (\input -> 
                                    case input of
                                      (s:ss) -> if f s
                                                  then Val s k
                                                  else Fails $ "expected: " ++ n ++ " (got " ++ sho s ++ ")"
                                      [] -> Fails "end of input (2)")
mkPar (Pure a) = Val a
mkPar (Star p q) = App . mkPar p . mkPar q
mkPar (Pipe p q) = \k -> mkPar p k `best` mkPar q k
mkPar (Fail m) = \fut -> Fails m

eof = Shift (\input -> case input of 
                         (s:ss) -> Fails "expected eof"
                         [] -> Done $ Val () $ Fails "Done")

parse p input = evalSteps ((mkPar p) eof) input

-----------------------------------
-- Incremental modifications

insertStr at ins s = l ++ ins ++ r 
    where (l,r) = splitAt at s

insert' at ins (r,over) 
    | at < getLength r = (insert at ins r, over)
    | otherwise = (r, insertStr at ins over)

insert :: Int -> [s] -> Result s a w r -> Result s a w r
insert at ins r@(Cha s err pr p)
    | at < length s     = Cha (insertStr at ins s) True pr p
    | otherwise         = Cha s                    err  pr (insert (at - length s) ins p)
insert at ins (Tip x p) = error "can't insert at Tip!"
insert at ins (Err _ x p) = error "can't insert at Err!"
insert at ins (Bin l x p err rl rr) 
    | at < getLength rl = Bin (l+length ins) x p True (insert at ins rl) rr
    | otherwise         = Bin (l+length ins) x p True rl (insert (at - getLength rl) ins rr)
    

delete' at len (r,over) = (delete at ll r, deleteStr atr lr over)
    where (ll, lr, atr) = dls at len (getLength r)

deleteStr at len s = l ++ drop len r
    where (l,r) = splitAt at s

delete :: Int -> Int -> Result s a w r -> Result s a w r
delete at 0   r = r
delete at len (Tip x p) = Tip x p
delete at len (Err m x p) = Err m x p
delete at len (Cha s err pr p) = Cha (deleteStr at ll s) (err || (ll > 0)) pr (delete atr lr p)
    where (ll, lr, atr) = dls at len (length s)
delete at len (Bin l x p err rl rr) = Bin (l-len) x p True (delete at ll rl) (delete atr lr rr)
    where (ll, lr, atr) = dls at len (getLength rl)

dls at len l0 = let ll = max 0 $ min len $ (l0-at) in (ll, len - ll, max 0 $ at - l0)

-----------------------------------
data Expr = AtomExpr Atom | BinExpr Expr Op Expr
           deriving (Typeable, Data, Show)

type Atom = Char
type Op = Char

type Parser = P Char

pExpr :: Parser Expr
pExpr = chainr1 pAtom pOp


pOp :: Parser (Expr -> Expr -> Expr)
pOp = pure (\op x y -> BinExpr x op y) <*> symbol "*" (`elem` "+-*/")

pAtom :: Parser Expr
pAtom = (pure AtomExpr <*> (symbol "a" isLetter))
       <|> parens pExpr

parens p = symbol "(" (== '(') *> p <* symbol ")" (== ')')

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> pure x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
 where
 scan = p <**> rest
 rest = fmap flip op <*> scan <|> pure id

----------

initial = (Cha "" True ((mkPar pExpr) eof) $ Tip (error "Initial") ((mkPar pExpr) eof), [])

-- updates :: [PResult Char Expr -> PResult Char Expr]
updates = [insert' 0 "a*b+c+d", repair, 
           insert' 2 "(", repair,
           insert' 6 ")", repair,
           insert' 2 "+", repair, 
           insert' 2 "x", repair, 
           insert' 0 "y+", repair, 
           insert' 8 "z", repair,
           insert' 8 "/", repair,
--           insert' 8 "*", repair,
           delete' 2 1, repair,
           delete' 2 1, repair
          ]


nacsr :: a -> [a -> a] -> [a]
nacsr k [] = [k]
nacsr k (f:fs) = k : nacsr (f k) fs

states = nacsr initial updates 

main = mapM_ (putStrLn . show) states

-- Properties:
-- prop_parser_consistent: parse (show t) = t
-- prop_partial: not (hasErr pResult) ==> getResult pResult == parse (getSymbols pResult)
-- prop_no_lost_edit: getSymbols (applyUpdate u pResult) = applyUpdate (getSymbols u pResult)


