--  Compiler Toolkit: Self-optimizing lexers
--
--  Author : Manuel M. T. Chakravarty
--  Created: 2 March 99
--
--  Version $Revision: 1.1 $ from $Date: 2002/07/28 03:35:20 $
--
--  Copyright (c) 1999 Manuel M. T. Chakravarty
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  Self-optimizing lexer combinators.
--
--  For detailed information, see ``Lazy Lexing is Fast'', Manuel
--  M. T. Chakravarty, in A. Middeldorp and T. Sato, editors, Proceedings of
--  Fourth Fuji International Symposium on Functional and Logic Programming,
--  Springer-Verlag, LNCS 1722, 1999.  (See my Web page for details.)
--
--             http://www.cse.unsw.edu.au/~chak/papers/Cha99.html
--
--  Thanks to Simon L. Peyton Jones <simonpj@microsoft.com> and Roman
--  Lechtchinsky <wolfro@cs.tu-berlin.de> for their helpful suggestions that
--  improved the design of this library.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The idea is to combine the benefits of off-line generators with
--  combinators like in `Parsers.hs' (which builds on Swierstra/Duponcheel's
--  technique for self-optimizing parser combinators).  In essence, a state
--  transition graph representing a lexer table is computed on the fly, to
--  make lexing deterministic and based on cheap table lookups.
--
--  Regular expression map to Haskell expressions as follows.  If `x' and `y'
--  are regular expressions,
--
--        -> epsilon
--    xy  -> x +> y
--    x*y -> x `star` y
--    x+y -> x `plus` y
--    x?y -> x `quest` y
--
--  Given such a Haskelized regular expression `hre', we can use
--
--    (1) hre `lexaction` \lexeme -> Nothing 
--    (2) hre `lexaction` \lexeme -> Just token
--    (3) hre `lexmeta`   \lexeme pos s -> (res, pos', s', Nothing)
--    (4) hre `lexmeta`   \lexeme pos s -> (res, pos', s', Just l)
--
--  where `epsilon' is required at the end of `hre' if it otherwise ends on
--  `star', `plus', or `quest', and then, we have
--
--    (1) discards `lexeme' accepted by `hre',
--    (2) turns the `lexeme' accepted by `hre' into a token,
--    (3) while discarding the lexeme accepted by `hre', transforms the
--        position and/or user state, and
--    (4) while discarding the lexeme accepted by `hre', transforms the
--        position and/or user state and returns a lexer to be used for the
--        next lexeme.
--
--  The component `res' in case of a meta action, can be `Nothing', `Just
--  (Left err)', or `Just (Right token)' to return nothing, an error, or a
--  token from a meta action, respectively.
--
--  * This module makes essential use of graphical data structures (for
--    representing the state transition graph) and laziness (for maintaining
--    the last action in `execLexer'.
--
--  NOTES:
--
--  * In this implementation, the combinators `quest`, `star`, and `plus` are
--    *right* associative - this was different in the ``Lazy Lexing is Fast''
--    paper.  This change was made on a suggestion by Martin Norbäck
--    <d95mback@dtek.chalmers.se>.
--
--- TODO ----------------------------------------------------------------------
--
--  * error correction is missing
--
--  * in (>||<) in the last case, `(addBoundsNum bn bn')' is too simple, as
--    the number of outgoing edges is not the sum of the numbers of the
--    individual states when there are conflicting edges, ie, ones labeled
--    with the same character; however, the number is only used to decide a
--    heuristic, so it is questionable whether it is worth spending the
--    additional effort of computing the accurate number
--
--  * Unicode posses a problem as the character domain becomes too big for
--    using arrays to represent transition tables and even sparse structures
--    will posse a significant overhead when character ranges are naively
--    represented.  So, it might be time for finite maps again.  
--
--    Regarding the character ranges, there seem to be at least two
--    possibilities.  Doaitse explicitly uses ranges and avoids expanding
--    them.  The problem with this approach is that we may only have
--    predicates such as `isAlphaNum' to determine whether a givne character
--    belongs to some character class.  From this representation it is
--    difficult to efficiently compute a range.  The second approach, as
--    proposed by Tom Pledger <Tom.Pledger@peace.com> (on the Haskell list)
--    would be to actually use predicates directly and make the whole business
--    efficient by caching predicate queries.  In other words, for any given
--    character after we have determined (in a given state) once what the
--    following state on accepting that character is, we need not consult the
--    predicates again if we memorise the successor state the first time
--    around.
--

module Yi.Ctk.Lexers (

   Regexp, Lexer, Action, 
   epsilon, char, (+>), 
   lexaction, lexactionErr, lexmeta, action, meta,
   (>|<), (>||<), 
   star, plus, quest, alt, string, LexerState, execLexer

   ) where 

import Prelude hiding (last)
import Maybe  (fromMaybe)
import Array  (Array, (!), assocs, accumArray)

import Yi.Ctk.DLists (zeroDL, snocDL, closeDL)

infixr 4 `quest`, `star`, `plus`
infixl 3 +>, `lexaction`, `lexmeta`, `action`, `meta`
infixl 2 >|<, >||<

-- constants
-- ---------

-- we use the dense representation if a table has at least the given number of 
-- (non-error) elements
--
denseMin :: Int
denseMin  = 20


-- data structures
-- ---------------

-- represents the number of (non-error) elements and the bounds of a table
--
type BoundsNum = (Int, Char, Char)

-- empty bounds
--
-- nullBoundsNum :: BoundsNum
-- nullBoundsNum  = (0, maxBound, minBound)

-- combine two bounds
--
addBoundsNum                            :: BoundsNum -> BoundsNum -> BoundsNum
addBoundsNum (n, lc, hc) (n', lc', hc')  = (n + n', min lc lc', max hc hc')

-- check whether a character is in the bounds
--
inBounds               :: Char -> BoundsNum -> Bool
inBounds c (_, lc, hc)  = c >= lc && c <= hc

-- Lexer errors
type Error = String

-- Lexical actions take a lexeme with its position and may return a token; in
-- a variant, an error can be returned (EXPORTED)
--
-- * if there is no token returned, the current lexeme is discarded lexing
--   continues looking for a token
--
type Action    t = String -> Maybe t
type ActionErr t = String -> Either Error t

-- Meta actions transform the lexeme, and a user-defined state; they
-- may return a lexer, which is then used for accepting the next token
-- (this is important to implement non-regular behaviour like nested
-- comments) (EXPORTED) 
--
type Meta s t = String -> s -> (Maybe (Either Error t), -- err/tok?
                                s,                      -- state
                                Maybe (Lexer s t))      -- lexer?

-- tree structure used to represent the lexer table (EXPORTED ABSTRACTLY) 
--
-- * each node in the tree corresponds to a state of the lexer; the associated 
--   actions are those that apply when the corresponding state is reached
--
data Lexer s t = Lexer (LexAction s t) (Cont s t)

-- represent the continuation of a lexer
--
data Cont s t = -- on top of the tree, where entries are dense, we use arrays
                --
                Dense BoundsNum (Array Char (Lexer s t))
                --
                -- further down, where the valid entries are sparse, we
                -- use association lists, to save memory (the first argument
                -- is the length of the list)
                --
              | Sparse BoundsNum [(Char, Lexer s t)]
                --
                -- end of a automaton
                --
              | Done
--            deriving Show

-- lexical action (EXPORTED ABSTRACTLY)
--
data LexAction s t = Action   (Meta s t)
                   | NoAction
--                 deriving Show

-- a regular expression (EXPORTED)
--
type Regexp s t = Lexer s t -> Lexer s t


-- basic combinators
-- -----------------

-- Empty lexeme (EXPORTED)
--
epsilon :: Regexp s t
epsilon  = id

-- One character regexp (EXPORTED) 
--
char   :: Char -> Regexp s t
char c  = \l -> Lexer NoAction (Sparse (1, c, c) [(c, l)])

-- Concatenation of regexps (EXPORTED)
--
(+>) :: Regexp s t -> Regexp s t -> Regexp s t
(+>)  = (.)

-- Close a regular expression with an action that converts the lexeme into a
-- token (EXPORTED)
--
-- * Note: After the application of the action, the position is advanced
--         according to the length of the lexeme.  This implies that normal
--         actions should not be used in the case where a lexeme might contain 
--         control characters that imply non-standard changes of the position, 
--         such as newlines or tabs.
--
action :: Regexp s t -> Action t -> Lexer s t
action = lexaction

lexaction      :: Regexp s t -> Action t -> Lexer s t
lexaction re a  = re `lexmeta` a'
  where
    a' lexeme s = 
       case a lexeme of
            Nothing -> (Nothing, s, Nothing)
            Just t  -> (Just (Right t), s, Nothing)

-- Variant for actions that may returns an error (EXPORTED)
--
lexactionErr      :: Regexp s t -> ActionErr t -> Lexer s t
lexactionErr re a  = re `lexmeta` a'
  where
     a' lexeme s = (Just (a lexeme), s, Nothing)

-- Close a regular expression with a meta action (EXPORTED)
--
-- * Note: Meta actions have to advance the position in dependence of the
--         lexeme by themselves.
--
meta :: Regexp s t -> Meta s t -> Lexer s t
meta = lexmeta

lexmeta      :: Regexp s t -> Meta s t -> Lexer s t
lexmeta re a  = re (Lexer (Action a) Done)

-- disjunctive combination of two regexps (EXPORTED)
--
(>|<)      :: Regexp s t -> Regexp s t -> Regexp s t
re >|< re'  = \l -> re l >||< re' l

-- disjunctive combination of two lexers (EXPORTED)
--
(>||<)                         :: Lexer s t -> Lexer s t -> Lexer s t
(Lexer a c) >||< (Lexer a' c')  = Lexer (joinActions a a') (joinConts c c')

-- combine two disjunctive continuations
--
joinConts :: Cont s t -> Cont s t -> Cont s t
joinConts Done c'   = c'
joinConts c    Done = c
joinConts c    c'   = let (bn , cls ) = listify c
                          (bn', cls') = listify c'
                      in
                      -- note: `addsBoundsNum' can, at this point, only
                      --       approx. the number of *non-overlapping* cases;
                      --       however, the bounds are correct 
                      --
                      aggregate (addBoundsNum bn bn') (cls ++ cls')
  where
    listify (Dense  n arr) = (n, assocs arr)
    listify (Sparse n cls) = (n, cls)
    listify _              = error "Lexers.listify: Impossible argument!"

-- combine two actions. Use the latter in case of overlap (!)
--
joinActions :: LexAction s t -> LexAction s t -> LexAction s t
joinActions NoAction a'       = a'
joinActions a        NoAction = a
joinActions _        a'       = a' -- error "Lexers.>||<: Overlapping actions!"

-- Note: `n' is only an upper bound of the number of non-overlapping cases
--
aggregate :: BoundsNum -> ([(Char, Lexer s t)]) -> Cont s t
aggregate bn@(n, lc, hc) cls
  | n >= denseMin = Dense  bn (accumArray (>||<) noLexer (lc, hc) cls)
  | otherwise     = Sparse bn (accum (>||<) cls)
  where
    noLexer = Lexer NoAction Done

-- combine the elements in the association list that have the same key
--
accum :: Eq a => (b -> b -> b) -> [(a, b)] -> [(a, b)]
accum _ []           = []
accum f ((c, el):ces) = 
  let (ce, ces') = gather c el ces
  in ce : accum f ces'
  where
    gather k e []                             = ((k, e), [])
    gather k e (ke'@(k', e'):kes) | k == k'   = gather k (f e e') kes
                                  | otherwise = let 
                                                  (ke'', kes') = gather k e kes
                                                in
                                                (ke'', ke':kes')


-- non-basic combinators
-- ---------------------

-- x `star` y corresponds to the regular expression x*y (EXPORTED)
--
star :: Regexp s t -> Regexp s t -> Regexp s t
--
-- The definition used below can be obtained by equational reasoning from this
-- one (which is much easier to understand): 
--
--   star re1 re2 = let self = (re1 +> self >|< epsilon) in self +> re2
--
-- However, in the above, `self' is of type `Regexp s t' (ie, a functional),
-- whereas below it is of type `Lexer s t'.  Thus, below we have a graphical
-- body (finite representation of an infinite structure), which doesn't grow
-- with the size of the accepted lexeme - in contrast to the definition using
-- the functional recursion.
--
star re1 re2  = \l -> let self = re1 self >||< re2 l
                      in 
                      self

-- x `plus` y corresponds to the regular expression x+y (EXPORTED)
--
plus         :: Regexp s t -> Regexp s t -> Regexp s t
plus re1 re2  = re1 +> (re1 `star` re2)

-- x `quest` y corresponds to the regular expression x?y (EXPORTED)
--
quest         :: Regexp s t -> Regexp s t -> Regexp s t
quest re1 re2  = (re1 +> re2) >|< re2

-- accepts a non-empty set of alternative characters (EXPORTED)
--
alt    :: [Char] -> Regexp s t
--
--  Equiv. to `(foldr1 (>|<) . map char) cs', but much faster
--
alt []  = error "Lexers.alt: Empty character set!"
alt cs  = \l -> let bnds = (length cs, minimum cs, maximum cs)
                in
                Lexer NoAction (aggregate bnds [(c, l) | c <- cs])

-- accept a character sequence (EXPORTED)
--
string    :: String -> Regexp s t
string []  = error "Lexers.string: Empty character set!"
string cs  = (foldr1 (+>) . map char) cs


-- execution of a lexer
-- --------------------

-- threaded top-down during lexing (current input, meta state) (EXPORTED)
--
type LexerState s = (String, s)

-- apply a lexer, yielding a token sequence and a list of errors (EXPORTED)
--
-- * Currently, all errors are fatal; thus, the result is undefined in case of 
--   an error (this changes when error correction is added).
--
-- * The final lexer state is returned.
--
-- * The order of the error messages is undefined.
--
execLexer :: Lexer s t -> LexerState s -> ([t], LexerState s, [Error])
--
-- * the following is moderately tuned
--
execLexer _ st@([], _) = ([], st, [])
execLexer l st = 
  case lexOne l st of
    (Nothing , _ , st') -> execLexer l st'
    (Just res, l', st') -> let (ts, final, allErrs) = execLexer l' st'
                           in case res of
                                (Left  err) -> (ts  , final, err:allErrs)
                                (Right t  ) -> (t:ts, final, allErrs)
  where
    -- accept a single lexeme
    --
    -- lexOne :: Lexer s t -> LexerState s t
    --        -> (Either Error (Maybe t), Lexer s t, LexerState s t)
    lexOne l0 st' = oneLexeme l0 st' zeroDL lexErr

      where
        -- the result triple of `lexOne' that signals a lexical error;
        -- the result state is advanced by one character for error correction
        --
        lexErr = let (cs, s) = st'
                     err = "Lexical error!\n" ++
                           "The character " ++ show (head cs) 
                              ++ " does not fit here; skipping it."
                 in
                 (Just (Left err), l, (tail cs, s))

        --
        -- we take an open list of characters down, where we accumulate the
        -- lexeme; this function returns maybe a token, the next lexer to use
        -- (can be altered by a meta action), the new lexer state, and a list
        -- of errors
        --
        -- we implement the "principle of the longest match" by taking a
        -- potential result quadruple down (in the last argument); the
        -- potential result quadruple is updated whenever we pass by an action 
        -- (different from `NoAction'); initially it is an error result
        --

        -- (dons): from the paper "we have to take the last possible
        -- action before the automaton gets stuck. .. when no further
        -- transition is possible, we execute the last action on the
        -- associated lexeme." This means that even once all possible
        -- actions have been looked at, we currently then wait till the
        -- _next_ char, before returning the last action we're carrying
        -- around. For interactive use, we instead need to execute the
        -- last action once we encounter it, not once we've gone past
        -- it. Hmm...

        --
        -- oneLexeme :: Lexer s t
        --           -> LexerState
        --           -> DList Char 
        --           -> (Maybe (Either Error t), Maybe (Lexer s t), 
        --               LexerState s t)
        --           -> (Maybe (Either Error t), Maybe (Lexer s t), 
        --               LexerState s t)
        --
        oneLexeme (Lexer a cont) state@(cs, s) csDL last =
            let last' = doaction a csDL state last
            in case cs of
                []      -> last'    -- at end, has to be this action
                (c:cs') -> oneChar cont c (cs', s) csDL last'   -- keep looking

        --
        -- There are more chars. Look at the next one
        --
        -- Now, if the next tbl is Done, then there is no more
        -- transition, so immediately execute our action
        --
        oneChar tbl c state csDL last = 
            case peek tbl c of
                Nothing              -> last
                Just (Lexer a Done)  -> doaction a (csDL `snocDL` c) state last
                Just l'              -> continue l' c state csDL last

        --
        -- Do the lookup.
        --
        peek Done _ = Nothing
        peek (Dense bn arr)  c | c `inBounds` bn = Just $ arr ! c
        peek (Sparse bn cls) c | c `inBounds` bn = lookup c cls
        peek _ _    = Nothing

        --
        -- continue within the current lexeme
        --
        continue l' c state csDL last = 
            oneLexeme l' state (csDL `snocDL` c) last

        --
        -- execute the action if present and finalise the current lexeme
        --
        doaction (Action f) csDL (cs, s) _last = 
            case f (closeDL csDL) s of
                (Nothing, s', l') 
                    | not . null $ cs -> lexOne (fromMaybe l0 l') (cs, s')
                (res, s', l')         -> (res, (fromMaybe l0 l'), (cs, s'))

        doaction NoAction _csDL _state last = last -- no change

