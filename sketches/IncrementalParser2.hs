{-# OPTIONS -fglasgow-exts #-}

module IncrementalParser2 where

import Data.Generics
import Data.Char
import Control.Applicative
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Tree

{- ----------------------------------------

Incremental parser prototype, 2nd version.

Key points:

- The basis is a mix between "Polish Parsers, Step by Step (Hughes and Swierstra)", 
  and "Parallel Parsing Processes (Claessen)"
  
  (It's strongly advised to read the papers!)

  => No LL(k) restriction on the grammar.

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


-- | A parser, on the applicative functor model (see Swierstra papers).
--
-- This data type used to construct a parser.  Each constructor represents one
-- of the five possible operators: 'symbol', 'fail', 'pure', choice and
-- application.
--
-- For parsing, values of this type (i.e., parsers) are transformed (compiled)
-- into parsing processes, of type 'Process'.  The corresponding function is
-- 'mkPar' but, in most cases this is done implicitly using the 'parse'
-- function.
-- 
-- A parser is parameterized over the symbol/token type @s@ and the parser
-- result type @a@.
-- 
data P s a where
    Symb :: (s -> String) -- ^ Print function for the matched symbol
         -> String        -- ^ Description of the symbol type
         -> (s -> Bool)   -- ^ Predicate
         -> P s s
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

-- | Matches any symbol for which the predicate function returns true.  The
-- string argument is used for describing the symbol type in error messages.
symbol :: Show s => String -> (s -> Bool) -> P s s
symbol s f = Symb show s f


-- | Interleaved parsing processes. This is a mix between "Polish Parsers,
-- Step by Step (Hughes and Swierstra)", and "Parallel Parsing Processes
-- (Claessen)".
--
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
-- represent @ex1@ (without using parentheses to resolve ambigouity) as:
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
-- where @$@ represents function application.  We can print the tree in
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
-- The parameter @r@ represents the type of the remaining of our expression.
data Steps s a r where
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    Shift :: ([s] -> Steps s a r)           -> Steps s a r
    Done  :: Steps s a r                    -> Steps s a r
    Fails :: [String]                       -> Steps s a r

-- For debugging:
instance Show (Steps x a s) where
    show (Val _ x) = "v" ++ show x
    show (App   x) = "." ++ show x
    show (Shift x) = ">" 
    show (Done  x) = "!" ++ show x
    show (Fails s) = "?" ++ show s

-- | Choose the non-failing option, (or the one that fails latest)
best :: Steps x a s -> Steps x a s ->  Steps x a s
Fails x `best` Fails y = Fails (nub $ x ++ y)
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

-- | Get the process' value (online)
evalSteps :: Steps s a (Steps s b r) -> [s] -> (a, Steps s b r, [s])
evalSteps (Val a s) xs = (a, s, xs)
evalSteps (Shift v) xs = evalSteps (v xs) (drop 1 xs)
evalSteps (Done v)  xs = evalSteps v xs
evalSteps (Fails s) xs = (error (show s), Fails s, xs)
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
    Una :: [s]  -> Maybe ErrorMessage -> Process s a w r -> Result s a w r -> Result s a w r  

-- For debugging:
instance Show (Result Char a w r) where
    showsPrec d r = -- shows (getLength r) .
                    -- (if hasErr r then showChar '?' else showChar ' ') .
                    case r of 
                      (Tip _ _) -> id
                      (Una s err _ p) -> (if isJust err then  showChar '?' else id) . 
                                         showString s . showsPrec 11 p
                      (Bin n _a _p _err l r) -> -- showParen (d > 10) $ 
                                                showsPrec 11 l . showsPrec 11 r



toTree :: Show s => Result s a w r -> Tree String
toTree (Tip _ _) = Node "." []
toTree (Una s err _ r) = Node (show s) [toTree r]
toTree (Bin _ _ _ _ l r) = Node "2" [toTree l, toTree r]

getResult :: Result s a w r -> a
getResult (Bin _ x _ _ _ _) = x
getResult (Tip x _) = x
getResult (Una _ _ _ x) = getResult x

getLength (Bin l _ _ _ _ _) = l
getLength (Tip _ _) = 0
getLength (Una s _ _ r) = length s + getLength r

getSymbols :: Result s a w r -> [s]
getSymbols (Bin _ _ _ _ l r) = getSymbols l ++ getSymbols r 
getSymbols (Tip _ _) = []
getSymbols (Una s _ _ r) = s ++ getSymbols r

hasErr (Bin _ _ _ err _ _) = err
hasErr (Tip _ _) = False
hasErr (Una _ err _ r) = isJust err || hasErr r
                     
-- | Get the process' "Result"
pEvalSteps :: Steps s a (Steps s b r) -> [s] -> (Result s a b r, Steps s b r, [s])
pEvalSteps p@(Val a s) xs = (Tip a p, s, xs)
pEvalSteps p@(Shift v) xs = let (proto, s', xs') = pEvalSteps (v xs) (drop 1 xs)
                            in (Una (take 1 xs) Nothing p proto, s', xs')
pEvalSteps p@(Done v)  xs = pEvalSteps v xs
pEvalSteps p@(Fails s) xs = (Una xs (Just msg) p $ Tip v p, Fails s, [])
    where msg = show s
          v = error msg
pEvalSteps p@(App s)   xs = (Bin (m + n) (f b) p (hasErr protoF || hasErr protoB) protoF protoB, s'', xs'')
    where (protoF,s',  xs')  = pEvalSteps s  xs
          (protoB,s'', xs'') = pEvalSteps s' xs'
          m = getLength protoF
          n = getLength protoB
          b = getResult protoB
          f = getResult protoF

fstrd (a,b,c) = (a,c)

repair' x = fst $ repair (x,[])
-- TODO: re-inject overhead if overhead is created.

-- | Repair a Result that's been marked with errors
repair :: (Result s a b r, [s]) -> (Result s a b r, [s])
repair (Tip x p, over) = (Tip x p, over)
repair c@(Una s err pr r, over) 
    | isJust err = fstrd (pEvalSteps pr (s ++ getSymbols r ++ over))
    | hasErr r = let (r', over') = repair (r,over)
                 in (Una s err pr r', over')
    | otherwise = c -- no error
repair (r@(Bin l x p err rl rr), over)
    -- no error => don't change anything.
    | not err = ((Bin l  x  p err rl  rr),  over)

    -- only right side has an eror, just re-parse it.
    | not (hasErr rl) = ((Bin l' x' p err' rl' rr'), over') 


    -- left side had a fixable error, we must re-parse the whole;
    -- boths sides have error, we must re-parse the whole
    | hasErr rl && not (hasErr rl') || hasErr rl && hasErr rr
        = fstrd $ pEvalSteps p (getSymbols r ++ over)
    -- If left is still in error after trying to fix it: we're better off
    -- keeping the local error, so don't change anything.
    | hasErr rl && hasErr rl' = ((Bin l  x  p err rl  rr),  over)
    where (rr', over') = repair (rr, over)
          (rl', _) = repair (rl, getSymbols rr ++ over)
          x' = (getResult rl) (getResult rr')
          l' = getLength rl + getLength rr'
          err' = hasErr rl || hasErr rr'


-- | Turns a parser specification into the corresponding parsing processes (in CPS)
mkPar :: P s a -> (forall b r.
                         (Steps s b r) ->
                         (Steps s a (Steps s b r)))
mkPar (Symb sho n f) = \k -> Shift (\input -> 
                                    case input of
                                      (s:ss) -> if f s
                                                  then Val s k
                                                  else Fails ["expected: " ++ n]
                                      [] -> Fails ["end of input (2)"])
mkPar (Pure a) = Val a
mkPar (Star p q) = App . mkPar p . mkPar q
mkPar (Pipe p q) = \k -> mkPar p k `best` mkPar q k
mkPar (Fail m) = \fut -> Fails [m]

terminalProcess x = Done $ Val x $ Fails ["Done"]

eof = Shift (\input -> case input of 
                         (s:ss) -> Fails ["expected eof"]
                         [] -> terminalProcess ())

parse p input = evalSteps ((mkPar p) eof) input

-----------------------------------
-- Incremental modifications

insertStr at ins s = l ++ ins ++ r 
    where (l,r) = splitAt at s

insert' at ins r = insert at ins r -- TODO: bounds check
-- TODO: this is a bit not nice because we don't have a way to distinguish
-- shifting the end of input and not shifting anything.

iMsg = Just "inserted stuff"

insert :: Int -> [s] -> Result s a w r -> Result s a w r
insert at ins r@(Una s err pr p)
    | length s > 0, at <= length s
        = Una (insertStr at ins s) iMsg pr p
    | otherwise
        = Una s                    err  pr (insert (at - length s) ins p)
insert at ins (Bin l x p err rl rr) 
    | getLength rl > 0, at <= getLength rl = Bin (l+length ins) x p True (insert at ins rl) rr
    | otherwise         = Bin (l+length ins) x p True rl (insert (at - getLength rl) ins rr)

delete' at len r = delete at ll r
    -- TODO: check bounds
    where (ll, lr, atr) = dls at len (getLength r)

deleteStr at len s = l ++ drop len r
    where (l,r) = splitAt at s

delete :: Int -> Int -> Result s a w r -> Result s a w r
delete at 0   r = r
delete at len (Una s err pr p) = Una (deleteStr at ll s) err' pr (delete atr lr p)
    where (ll, lr, atr) = dls at len (length s)
          err' = if ll == 0 || isJust err then err else Just "deleted stuff"
delete at len (Bin l x p err rl rr) = Bin (l-len) x p True (delete at ll rl) (delete atr lr rr)
    where (ll, lr, atr) = dls at len (getLength rl)

dls at len l0 = let ll = max 0 $ min len $ (l0-at) in (ll, len - ll, max 0 $ at - l0)

-----------------------------------
data Expr = Var String 
          | Lit String
          | BinExpr Expr Op Expr
           deriving (Typeable, Data, Show)

data Decl = Decl String Expr

type Op = Char

type Parser = P Char

pDecl :: Parser ()
pDecl = Decl <$> pId <* tok "=" <*> pExpr



pExpr :: Parser Expr
pExpr = chainr1 pAtom pOp


-- many p = empty <|> many1 p

space = symbol " " isSpace

comment = () <$ string "--" <* many (symbol "." (/='\n')) <* symbol "n" (== '\n')

skipMany :: Alternative f => f a -> f ()
skipMany v = skipMany_v
  where skipMany_v = some_v <|> pure ()
	some_v = v *> skipMany_v

munch :: Alternative f => f a -> f ()
munch v = many_v
  where many_v = some_v <|> pure ()
	some_v = v *> many_v


token x = skipMany space *> x

tok s = token (string s)

string [] = pure ()
string (x:xs) = symbol [x] (== x) *> string xs

number = token $ some (symbol "9" isDigit)

pId = token $ some (symbol "a" isLetter)

pOp :: Parser (Expr -> Expr -> Expr)
pOp = token $ pure (\op x y -> BinExpr x op y) <*> symbol "*" (`elem` "+-*/")

pAtom :: Parser Expr
pAtom =     Var <$> pId 
        <|> Lit <$> number
        <|> parens pExpr

parens p = tok "(" *> p <* tok ")"

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> pure x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
 where
 scan = p <**> rest
 rest = fmap flip op <*> scan <|> pure id

----------

initial = Una " " iMsg ((mkPar pExpr) eof) $ 
          Tip (error "Initial") ((mkPar pExpr) eof)

-- updates :: [PResult Char Expr -> PResult Char Expr]
updates = [insert' 0 "a*b+c+d", repair', 
           insert' 2 "(", repair',
           insert' 6 ")", repair',
           insert' 2 "+", repair', 
           insert' 2 "x", repair', 
           insert' 0 "y+", repair', 
           insert' 8 "/", repair',
           insert' 9 "z", repair',
--           insert' 8 "*", repair',
           delete' 2 1, repair',
           delete' 2 1, repair'
          ]


nacsr :: a -> [a -> a] -> [a]
nacsr k [] = [k]
nacsr k (f:fs) = k : nacsr (f k) fs

states = nacsr initial updates 

main_test = mapM_ (putStrLn . show) states

-- Properties:
-- prop_parser_consistent: parse (show t) = t
-- prop_partial: not (hasErr pResult) ==> getResult pResult == parse (getSymbols pResult)
-- prop_no_lost_edit: getSymbols (applyUpdate u pResult) = applyUpdate (getSymbols u pResult)


