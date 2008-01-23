{-# LANGUAGE GADTs #-}

module Main where

import Control.Applicative
import Data.Char

{- ----------------------------------------

Incremental parser prototype.

Key points:

- The basis is a simple LL(k) parser.

  It's a simple "list of successes" parsing combinators implementation, where we cut the
  backtracking at a certain lookahead. I expect the performance to be ok since it
  can work "online" and incrementally (see below)

  Unfortunately, a precise haskell grammar would have infinite lookahead. I hope we can
  get by with a "coarse" grammar.

  Another option (fixing the above problems) would be to use "Polish Parsers" as a base.

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


-----------------------------------------
-- Parser representation

-- | A parser, on the applicative functor model (see Swierstra papers)
data P s a where
    Symb :: String -> (s -> Bool) -> P s s
    Fail :: String -> P s a
    Pure :: a -> P s a
    Pipe :: P s a -> P s a -> P s a
    Star :: P s (a -> b) -> P s a -> P s b

-- data P s a = Symb String (s -> Bool) (s -> a) | Fail String | Pure a | Pipe (P s a) (P s a) | forall b. Star (P s (b->a)) (P s b)

instance Show (P s a) where
    showsPrec _ (Symb s _) = showString s
    showsPrec _ (Fail s) = showString s
    showsPrec _ (Pure _) = showString "."
    showsPrec d (Pipe a b) = showParen (d > 1) $ showsPrec 1 a . showString " <|> " . showsPrec 1 b
    showsPrec d (Star f x) = showParen (d > 2) $ showsPrec 2 f . showString " <*> " . showsPrec 2 x


-- Primitive operations.


instance Functor (P s) where
    fmap f p = pure f <*> p

instance Applicative (P s) where
    pure = Pure
    (<*>) = Star

instance Alternative (P s) where
    empty = Fail "no parse"
    (<|>) = Pipe

symbol :: String -> (Char -> Bool) -> Parser Char
symbol s f = Symb s f


-------------------------------------------------
-- Partial results.

-- This type represents the (possibly partial) result of a parse.
-- We cache many intermediate results so incremental parsing is possible

type PResult s a = (Result s a, [s]) -- a partial result + leftover.

data Result s a where
    Bin :: Int                                 -> -- length of the thing (#syms)
           a                                   -> -- the value
           P s a                               -> -- material to fully re-build.
           Bool                                -> -- contains some error node?
           Result s (b -> a) -> Result s b -> -- material to partially re-build.
           Result s a
    Tip :: [s] -> a -> P s a -> Result s a    -- An atomic value
    Err :: [s] -> a -> P s a -> Result s a    --


-- Same in non GADT syntax:
--
-- data Result s a = forall b. Bin Int a (P s a) Bool (Result s (b -> a)) (Result s b)
--                 | Tip [s] a (P s a)
--                 | Err [s] a (P s a)



instance (Show s) => Show (Result s a) where
    showsPrec d r = --(shows (getLength r)).
                    -- (if hasErr r then showChar '?' else showChar ' ') .
                    case r of
                      (Tip s _ _) -> showsPrec 11 s
                      (Err s _ _) -> showChar '?' . showsPrec 11 s
                      (Bin _ _a _p _err l s) -> showParen (d > 10) $ showsPrec 11 l . showString " " . showsPrec 10 s

getResult :: Result s a -> a
getResult (Bin _ x _ _ _ _) = x
getResult (Tip _ x _) = x
getResult (Err _ x _) = x

getLength :: Result t1 t -> Int
getLength (Bin l _ _ _ _ _) = l
getLength (Tip s _ _) = length s
getLength (Err s _ _) = length s

getParser :: Result t1 t -> P t1 t
getParser (Bin _ _ p _ _ _) = p
getParser (Err _ _ p) = p
getParser (Tip _ _ p) = p

getSymbols :: Result s a -> [s]
getSymbols (Bin _ _ _ _ l r) = getSymbols l ++ getSymbols r
getSymbols (Tip s _ _) = s
getSymbols (Err s _ _) = s

hasErr :: Result t1 t -> Bool
hasErr (Bin _ _ _ err _ _) = err
hasErr (Err _ _ _) = True
hasErr (Tip _ _ _) = False


----------------------------------------------------------------
-- Edition of the partial result


insertStr :: Int -> [a] -> [a] -> [a]
insertStr at ins s = l ++ ins ++ r
    where (l,r) = splitAt at s

insert' :: Int -> [s] -> PResult s a -> PResult s a
insert' at ins (r,over)
    | at < getLength r = (markErr (at - lookahead) (length ins + lookahead) $
                          insert at ins r, over)
    | otherwise = (markErr (getLength r - lookahead) lookahead r, insertStr at ins over)

insert :: Int -> [s] -> Result s a -> Result s a
insert at ins (Tip s x p) = Err (insertStr at ins s) x p
insert at ins (Err s x p) = Err (insertStr at ins s) x p
insert at ins (Bin l x p _ rl rr)
    | at < getLength rl = Bin (l+length ins) x p True (insert at ins rl) rr
    | otherwise         = Bin (l+length ins) x p True rl (insert (at - getLength rl) ins rr)


-- delete' :: Int -> Int -> PResult s a -> PResult s a
-- delete' at len (

deleteStr :: Int -> Int -> [a] -> [a]
deleteStr at len s = l ++ drop len r
    where (l,r) = splitAt at s

delete :: Int -> Int -> Result s a -> Result s a
delete _ 0   r = r
delete at len (Tip s x p) = Err (deleteStr at len s) x p
delete at len (Err s x p) = Err (deleteStr at len s) x p
delete at len (Bin l x p _ rl rr) = Bin (l-len) x p True (delete at ll rl) (delete (at - getLength rl) lr rr)
    where ll = min (getLength rl) len
          lr = max (len - getLength rl) 0


markErr :: Int -> Int -> Result s a -> Result s a
markErr at len r
    | outsideZone at len r = r
    | insideZone  at len r = Err (getSymbols r) (getResult r) (getParser r)
    | otherwise = case r of
                    Tip l x p -> Err l x p
                    Err l x p -> Err l x p
                    Bin l x p _ rl rr -> Bin l x p True (markErr at len rl) (markErr (at - getLength rl) len rr)

insideZone, outsideZone :: Int -> Int -> Result t t1 -> Bool
insideZone  at len r = at      <= 0 && getLength r <= at + len
outsideZone at len r = at + len < 0 || getLength r <  at


----------------------------------------------------------------


----------------------------------------------------------------
-- Parsing


-- | Test if a parser can accept the given lookahead
test :: P s a -> [s] -> Bool
test p s = not $ null (check p lookahead s)

check :: P s a -> Int -> [s] -> [(Int, [s])]
check _            0 _      = [(0, [])]
check (Fail _)     _ _      = []
check (Symb _ _)   _ []     = []
check (Symb _ f)   n (x:xs) = if f x then [(n - 1, xs)] else []
check (Pure _)     n xs     = [(n, xs)]
check (Pipe p q)   n xs     = check p n xs ++ check q n xs
check (Star p q)   n xs     = [(n'', xs'')
                            | ~(n',xs') <- check p n xs,
                              ~(n'',xs'') <- check q n' xs']


-- | Partially parse the input
pexec :: P s a -> [s] -> PResult s a
pexec r@(Symb _ _  )   []     = (Err [] (error "end of input") r, [])
pexec r@(Symb s f  )   (x:xs) = if f x then (Tip [x] x r,xs) else (Err [] (error ("expected " ++ s)) r, (x:xs))
pexec r@(Fail msg) xs     = (Err [] (error msg) r, xs)
pexec r@(Pure x)   xs     = (Tip [] x r, xs)
pexec (Pipe p q) xs     = pexec (if not (test p xs) then q else p) xs
pexec r@(Star p q) xs     = (Bin (m + n) (f x) r (hasErr protoF || hasErr protoX) protoF protoX, xs'')
                              where (protoF,xs')  = pexec p xs
                                    (protoX,xs'') = pexec q xs'
                                    m = getLength protoF
                                    n = getLength protoX
                                    x = getResult protoX
                                    f = getResult protoF


-- | Repairs a Result that's been marked with errors
repair :: PResult s a -> PResult s a
repair ((Tip s x p),over) = (Tip s x p, over)
repair ((Err s _ p),over) = pexec p (s ++ over)
repair ((Bin l x p err rl rr), over)
    | not err    = ((Bin l  x  p err rl  rr),  over)
    -- if left is still in error, don't bother reparsing (we're as good with the current parse)
    | hasErr rl' = ((Bin l  x  p err  rl  rr),  over)
    | otherwise  = ((Bin l' x' p err' rl' rr'), over')
    where (rl', overl) = repair (rl, getSymbols rr ++ over)
          (rr', over') = if getLength rl == getLength rl'
                         then repair (rr, over)
                         else pexec (getParser rr) overl
                         -- when the length of left is different, we cannot re-use the parser for right side.
          x' = (getResult rl') (getResult rr')
          l' = getLength rl' + getLength rr'
          err' = hasErr rl' || hasErr rr'



-------------------------------
-- Example
lookahead :: Int
lookahead = 2

-- Data type
data Expr = AtomExpr Atom | BinExpr Expr Op Expr
            deriving (Show)

type Atom = Char
type Op = Char

type Parser = P Char

pExpr :: Parser Expr
pExpr = chainr1 pAtom pOp

pOp :: Parser (Expr -> Expr -> Expr)
pOp = pure (\op x y -> BinExpr x op y) <*> symbol "*" (`elem` "+-*/")

pAtom :: Parser Expr
pAtom = pure AtomExpr <*> symbol "v" isLetter

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> pure x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
 where
  scan = p <**> rest
  rest = fmap flip op <*> scan <|> pure id


initial :: (Result Char Expr, [a])
initial = (Err [] (error "initial") pExpr, [])

updates :: [PResult Char Expr -> PResult Char Expr]
updates = [insert' 0 "a+b+c", repair, insert' 2 "+", repair, insert' 2 "x", repair, insert' 0 "y+", repair, insert' 7 [error "ohch"], repair]


nacsr :: a -> [a -> a] -> [a]
nacsr k [] = [k]
nacsr k (f:fs) = k : nacsr (f k) fs

states :: [(Result Char Expr, [Char])]
states = nacsr initial updates

main :: IO ()
main = mapM_ (putStrLn . show) states


