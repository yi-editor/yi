{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, DeriveDataTypeable,
             Rank2Types, PatternGuards #-}
{-# OPTIONS -Wall #-}
module IncrementalParser2 where

import Data.Generics
import Data.Char
import Control.Applicative
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Tree

import Debug.Trace

{- ----------------------------------------

Incremental parser prototype, 2nd version.

Key points:

- The basis is a mix between "Polish Parsers, Step by Step (Hughes and Swierstra)", 
  and "Parallel Parsing Processes (Claessen)"
  
  (It's strongly advised to read the papers!)

  => No LL(k) restriction on the grammar.

- The parser has "online" behaviour.

  This is a big advantage because we don't have to parse the whole file to
  begin syntax highlight the beginning of it.

- Resilient to insert parse errors. 

  The structure around the inserted error is preserved. This would allow
  things like "wiggly underlining" of errors in realtime; also things like
  indentation, etc. can continue working at a coarse level in presence of
  errors.

- Based on Applicative functors.

  This is not as powerful as Monadic parsers, but easier to work with. This is
  needed if we want to build the result lazily.


-------------------------------------------}


-- | A parser, based on the applicative functor model (see Swierstra papers).
--
-- This data type is used to construct a parser.  Each constructor represents
-- one of the five possible operators: 'symbol', 'fail', 'pure', choice and
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
    LkAh :: (Maybe s -> Maybe a) -> P s a
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
data Steps s a r where
    Val   :: a -> Steps s b r               -> Steps s a (Steps s b r)
    -- ^ The process that returns the value of type @a@ which is followed by a
    -- parser returning a value of type @b@.
    App   :: Steps s (b -> a) (Steps s b r) -> Steps s a r
    -- ^ Takes a process that returns a function @f@ of type @b -> a@ and is
    -- followed by a process returning a value @x@ of type @b@.  The resulting
    -- process will return the result of applying the function @f@ to @x@.
    Shift :: ([s] -> Steps s a r)           -> Steps s a r
    -- ^ This is the only process that consumes any input.  It takes a
    -- function that takes the full input and returns a parser to use for the
    -- rest of the input.
    --
    -- [XXX: I'm not sure why it takes a full list.  It seems to be an
    -- implicit assumption, that only one element is being consumed.  Having
    -- type @Maybe s@ should be sufficient ('Maybe' because we might have
    -- EOF.)  Alternatively, the function argument could return the input it
    -- didn't consume, but that could complicate functions such as 'best'.]
    -- [JP: Correct, one should replace [s] by Maybe s]
    Look :: ([s] -> Steps s a r)       -> Steps s a r
    Done  :: Steps s a r                    -> Steps s a r
    -- ^ The parser that signals success.  The argument is the continuation.
    Fails :: [String]                       -> Steps s a r
    -- ^ The parser that signals failure.  The argument is a list of error
    -- messages.  It is a list, so that we can combine multiple error
    -- messages.


-- For debugging:
instance Show (Steps x a s) where
    show (Val _ x) = "v" ++ show x
    show (App   x) = "." ++ show x
    show (Shift _) = ">" 
    show (Look _)  = "*"
    show (Done  x) = "!" ++ show x
    show (Fails s) = "?" ++ show s
{-
newtype PR s a = PR { unPR :: forall k . ([s] -> k) -> ([s] -> Steps s a k) }

data Q s a = Q { proc  :: PR s a
               , zerop :: Maybe (Bool, Q s a)
                 -- ^ Result for 'zeroP' and whether the parser is a 'pLow'.
               }

-- ^ Return @Nothing@ if the parser cannot parse the empty
-- string.  Return @Just p@ if it can, where @p@ is a parser
-- that parses the empty string.
zeroP :: Q s a -> Maybe (Q s a)
zeroP (Q _ (Just (_,p))) = Just p
zeroP _ = Nothing

qSucceed x = let r = Q p (Just (False, r)) in r
  where p = PR $ \k inp -> 
-}
-- | Choose the non-failing option, (or the one that fails latest)
best :: Steps x a s -> Steps x a s ->  Steps x a s
--l `best` r | trace ("best: "++show (l,r)) False = undefined
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
getProgress f (Look s)  = Look (\input -> f (s input))

-- | Get the process' value (online).  Returns value, continuation and
--   remaining input.
evalSteps :: Steps s a (Steps s b r) -> [s] -> (a, Steps s b r, [s])
evalSteps (Val a s) xs = (a, s, xs)
evalSteps (Shift v) xs = evalSteps (v xs) (drop 1 xs)
evalSteps (Look v)  xs = evalSteps (v xs) xs
evalSteps (Done v)  xs = evalSteps v xs
evalSteps (Fails s) xs = (error (show s), Fails s, xs)
evalSteps (App s)   xs = let (f,s',  xs')  = evalSteps s  xs
                             (a,s'', xs'') = evalSteps s' xs'
                         in (f a, s'', xs'')

evalSteps' :: Process s a b r -> [s] -> Either [ErrorMessage] (a, Steps s b r, [s])
evalSteps' (Val a k) xs = Right (a, k, xs)
evalSteps' (Shift k) xs = evalSteps' (k xs) (drop 1 xs)
evalSteps' (Look k)  xs = evalSteps' (k xs) xs
evalSteps' (Done k)  xs = evalSteps' k xs
evalSteps' (Fails e) xs = Left e
evalSteps' (App k)   xs = evalSteps' k xs `bind` (\(f, k', xs') ->
                          evalSteps' k' xs' `bind` (\(a, k'', xs'') ->
                          Right (f a, k'', xs'')))
  where
    bind :: Either e a -> (a -> Either e b) -> Either e b 
    Left x `bind` _ = Left x
    Right x `bind` m = m x

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
         (Una s err _ p) -> (if isJust err 
                             then (\q -> showBraces True q) 
                             else id) 
                            (showString s . showsPrec 11 p)
         (Bin n _a _p _err l r) -> -- showParen (d > 10) $ 
                                                showsPrec 11 l . showsPrec 11 r

showBraces b p = if b then showChar '{' . p . showChar '}' else p

toTree :: Show s => Result s a w r -> Tree String
toTree (Tip _ _) = Node "." []
toTree p@(Una s err _ r) = Node (show s++if hasErr p then "?" else "") [toTree r]
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
pEvalSteps :: Show s => Process s a b r -> [s] -> (Result s a b r, Steps s b r, [s])
--pEvalSteps p xs | trace ("eval: "++show (p, xs)) False = undefined
pEvalSteps p@(Val a s) xs = (Tip a p, s, xs)
pEvalSteps p@(Shift v) xs = let (proto, s', xs') = pEvalSteps (v xs) (drop 1 xs)
                            in (Una (take 1 xs) Nothing p proto, s', xs')
pEvalSteps p@(Look v)  xs = pEvalSteps (v xs) xs
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
repair :: Show s => (Result s a b r, [s]) -> (Result s a b r, [s])
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
-- a symbol is parsed by 
mkPar (Symb sho n f) = \k -> Shift (\input -> 
                                    case input of
                                      (s:ss) -> if f s
                                                  then Val s k
                                                  else Fails ["expected: " ++ n]
                                      [] -> Fails ["end of input (2)"])
mkPar (LkAh f) = \k -> Look (\input ->
                                 case input of
                                   (s:ss) -> f' (Just s) k
                                   [] -> f' Nothing k)
  where f' s k = case f s of
                   Nothing -> Fails ["look-ahead fail"]
                   Just a  -> Val a k
mkPar (Pure a) = Val a
mkPar (Star p q) = App . mkPar p . mkPar q
mkPar (Pipe p q) = \k -> mkPar p k `best` mkPar q k
mkPar (Fail m) = \fut -> Fails [m]

terminalProcess x = Done $ Val x $ Fails ["Done"]

eof :: Show s => Steps s () (Steps s b r)
eof = Shift (\input -> --trace ("[eof: "++show input++"]") $
                       case input of 
                         (s:ss) -> Fails ["expected eof"]
                         [] -> terminalProcess ())

--parse :: P s a -> [s] -> (a, Steps s () (Steps s b r), [s])
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
    | l > 0, at <= l
        = Una (insertStr at ins s) iMsg pr p
    | otherwise
        = Una s                    err  pr (insert (at - l) ins p)
  where l = length s
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
delete _ _ (Tip _ _) = undefined 

dls at len l0 = let ll = max 0 $ min len $ (l0-at) in (ll, len - ll, max 0 $ at - l0)

-----------------------------------
data Expr = Var String 
          | Lit String
          | BinExpr Expr Op Expr
           deriving (Typeable, Data, Show)

data Decl = Decl String Expr

type Op = Char

type Parser = P Char

pDecl :: Parser Decl
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

--notFollowedBy :: Parser a -> Parser a
--notFollowedBy p = ?

lookAhead :: (Maybe Char -> Maybe a) -> Parser a
lookAhead f = LkAh f

--many_gr :: Parser a -> 

--manyCharGr :: (Char -> Bool) -> Parser a
--manyCharGr =

----------

initial = Una " " iMsg ((mkPar p) eof) $ 
          Tip (error "Initial") ((mkPar p) eof)
  where p = pExpr

-- updates :: [PResult Char Expr -> PResult Char Expr]
updates = [insert' 0 "a*b+c+d", repair' ,
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
          ] -- -}


nacsr :: a -> [a -> a] -> [a]
nacsr k [] = [k]
nacsr k (f:fs) = k : nacsr (f k) fs

states = nacsr initial updates 

main_test = mapM_ (putStrLn . show) states

-- Properties:
-- prop_parser_consistent: parse (show t) = t
-- prop_partial: not (hasErr pResult) ==> getResult pResult == parse (getSymbols pResult)
-- prop_no_lost_edit: getSymbols (applyUpdate u pResult) = applyUpdate (getSymbols u pResult)

sepBy :: (Alternative f) => f a -> f b -> f [a]
sepBy p s = sepBy1 p s <|> pure []

sepBy1 :: (Alternative f) => f a -> f b -> f [a]
sepBy1 p s = (:) <$> p <*> many (s *> p)

tst_ex1 = do --putStrLn $ drawTree (toTree r)
             --print $ p eof
             --print $ getResult r
             --print $ getSymbols r
             print (r, k, s)
             --print $ evalSteps' (p eof) inp
  where
    p = mkPar $ hVarId `sepBy` some space
    inp = "  xyz yz" 
    (r, k, s) = pEvalSteps (p eof) inp

mynum = token (some (symbol "digit" isDigit))

data BinTree = BN BinTree BinTree | Leaf Char deriving Show

leaf = Leaf <$> symbol "character" (isDigit)
node = BN <$> leaf <*> leaf

--many' p = 

--manySym_gr :: (Char -> Bool) -> (Char -> Bool) -> Parser a

notFollowedBy :: Parser a -> (Char -> Bool) -> Parser a
notFollowedBy p f = id <$> p <* lookAhead f'
  where f' Nothing = Just ()
        f' (Just x) | f x       = Just ()
                    | otherwise = Nothing
   
------------------------------------------------------------------------------

hChar c = symbol (show c) (==c)
hSmall = symbol "lower-case letter" isLower <|> hChar '_'
hLarge = symbol "upper-case letter" isUpper
hLetter = symbol "letter" isLetter <|> hChar '_'
hDigit = symbol "digit" isDigit

hVarId = token $
    (:) <$> hSmall <*> many (hLetter <|> hDigit <|> hChar '\'')
  
