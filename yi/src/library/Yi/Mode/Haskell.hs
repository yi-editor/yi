{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving
           , Rank2Types #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Haskell-specific modes and commands.
module Yi.Mode.Haskell
  (
   -- * Modes
   haskellAbstract,
   cleverMode,
   preciseMode,
   literateMode,
   fastMode,

   -- * IO-level operations
   ghciGet,
   ghciSend,
   ghciLoadBuffer,
   ghciInferType,
  ) where

import Prelude hiding (and,error,elem,notElem,all,concatMap,exp)
import Data.Maybe (listToMaybe, isJust, catMaybes)
import Data.Default
import Data.Foldable
import Data.Typeable
import Data.Binary
import Control.Applicative
import Control.Monad hiding (forM_)
import Yi.Core
import Yi.File
import Yi.Lexer.Alex (Tok(..),Posn(..),tokBegin,tokEnd,tokRegion)
import Yi.String
import Yi.Syntax
import qualified Yi.Syntax.Driver as Driver
import Yi.Syntax.Haskell as Hask
import Yi.Syntax.Strokes.Haskell as HS
import Yi.Syntax.Paren as Paren
import Yi.Syntax.Tree
import Yi.Syntax.OnlineTree as OnlineTree
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.LiterateHaskell as LiterateHaskell
import Yi.Lexer.Haskell as Haskell
import qualified Yi.Mode.GHCi as GHCi
import qualified Yi.Mode.Interactive as Interactive
import Yi.Modes (anyExtension, extensionOrContentsMatch)
import Yi.MiniBuffer
import Yi.Debug
import Yi.Monad
import Yi.Utils

haskellAbstract :: Mode (tree TT)
haskellAbstract = emptyMode
  {
     modeApplies = extensionOrContentsMatch extensions shebangPattern,
     modeName = "haskell",
     modeToggleCommentSelection = toggleCommentSelectionB "-- " "--"
  }
     {-
     Some of these are a little questionably haskell
     related. For example ".x" is an alex lexer specification
     I dare say that there are other file types that use ".x"
     as the file extension.
     For now though this is probably okay given the users of
     'yi' are mostly haskell hackers, as of yet. -}
    where extensions = ["hs", "x", "hsc", "hsinc"]
          shebangPattern = "^#![[:space:]]*/usr/bin/env[[:space:]]+runhaskell"

-- | "Clever" haskell mode, using the paren-matching syntax.
cleverMode :: Mode (Paren.Tree (Tok Haskell.Token))
cleverMode = haskellAbstract
  {
    modeIndent = cleverAutoIndentHaskellB,
    modeGetStrokes = \t point begin end -> Paren.getStrokes point begin end t,
    modeHL = ExtHL $
    Driver.mkHighlighter (skipScanner 50 . IncrParser.scanner Paren.parse . Paren.indentScanner . haskellLexer)

  , modeAdjustBlock = adjustBlock
  , modePrettify = (cleverPrettify . allToks)
  , modeGetAnnotations = tokenBasedAnnots Paren.tokenToAnnot

 }

fastMode :: Mode (OnlineTree.Tree TT)
fastMode = haskellAbstract
  {
    modeName = "fast haskell",
    modeHL = ExtHL $
    Driver.mkHighlighter (IncrParser.scanner OnlineTree.manyToks . haskellLexer),
    modeGetStrokes = tokenBasedStrokes Paren.tokenToStroke,
    modeGetAnnotations = tokenBasedAnnots Paren.tokenToAnnot
 }

literateMode :: Mode (Paren.Tree TT)
literateMode = haskellAbstract
  { modeName = "literate haskell"
  , modeApplies = anyExtension ["lhs"]
  , modeHL = ExtHL $
    Driver.mkHighlighter (IncrParser.scanner Paren.parse . Paren.indentScanner . literateHaskellLexer)
  , modeGetStrokes = \t point begin end -> Paren.getStrokes point begin end t
  , modeGetAnnotations = \t _begin -> catMaybes $ fmap Paren.tokenToAnnot $ allToks t -- FIXME I think that 'begin' should not be ignored
  , modeAdjustBlock = adjustBlock
  , modeIndent = cleverAutoIndentHaskellB
  , modePrettify = cleverPrettify . allToks }

-- | Experimental Haskell mode, using a rather precise parser for the syntax.
preciseMode :: Mode (Hask.Tree TT)
preciseMode = haskellAbstract
  {
    modeName = "precise haskell"
  , modeIndent = cleverAutoIndentHaskellC
  , modeGetStrokes = \ast point begin end -> HS.getStrokes point begin end ast
  , modeHL = ExtHL $
      Driver.mkHighlighter (IncrParser.scanner Hask.parse . Hask.indentScanner . haskellLexer)
  , modePrettify = cleverPrettify . allToks

--   , modeGetAnnotations = (tokenBasedAnnots Hask.tokenToAnnot) . getExprs
 }


haskellLexer :: Scanner Point Char -> Scanner (Alex.AlexState Haskell.HlState) (Tok Token)
haskellLexer = Alex.lexScanner Haskell.alexScanToken Haskell.initState

literateHaskellLexer :: Scanner Point Char -> Scanner (Alex.AlexState LiterateHaskell.HlState) (Tok Token)
literateHaskellLexer = Alex.lexScanner LiterateHaskell.alexScanToken LiterateHaskell.initState

adjustBlock :: Paren.Tree (Tok Token) -> Int -> BufferM ()
adjustBlock e len = do
  p <- pointB
  l <- curLn
  let t = Paren.getIndentingSubtree e p l
  case t of
    Nothing -> return ()
    Just it ->
           savingExcursionB $ do
                   let (_startOfs, height) = Paren.getSubtreeSpan it
                   col <- curCol
                   forM_ [1..height] $ const $ do
                               lineDown
                               indent <- indentOfB =<< readLnB
                               -- it might be that we have 1st column comments in the block,
                               -- which should not be changed.
                               when (indent > col) $
                                if len >= 0
                                 then do insertN (replicate len ' ')
                                         leftN len
                                 else do
                                    deleteN (negate len)

-- | Returns true if the token should be indented to look as "inside" the group.
insideGroup :: Token -> Bool
insideGroup (Special c) = c `notElem` "',;})]"
insideGroup _ = True

cleverAutoIndentHaskellB :: Paren.Tree TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellB e behaviour = do
  indentSettings <- indentSettingsB
  let indentLevel = shiftWidth indentSettings
  previousIndent <- indentOfB =<< getNextNonBlankLineB Backward
  nextIndent     <- indentOfB =<< getNextNonBlankLineB Forward
  solPnt <- pointAt moveToSol
  eolPnt <- pointAt moveToEol
  let onThisLine ofs = ofs >= solPnt && ofs <= eolPnt
      firstTokNotOnLine = listToMaybe .
                              filter (not . onThisLine . posnOfs . tokPosn) .
                              filter (not . isErrorTok . tokT) . concatMap allToks
  let stopsOf :: [Paren.Tree TT] -> [Int]
      stopsOf (g@(Paren.Paren open ctnt close):ts')
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [groupIndent open ctnt]  -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts' -- this group is closed before this line; just skip it.
      stopsOf ((Paren.Atom (Tok {tokT = t})):_) | startsLayout t = [nextIndent, previousIndent + indentLevel]
        -- of; where; etc. we want to start the block here.
        -- Also use the next line's indent:
        -- maybe we are putting a new 1st statement in the block here.
      stopsOf ((Paren.Atom _):ts) = stopsOf ts
         -- any random part of expression, we ignore it.
      stopsOf (t@(Paren.Block _):ts) = shiftBlock + maybe 0 (posnCol . tokPosn) (getFirstElement t) : stopsOf ts
      stopsOf (_:ts) = stopsOf ts
      stopsOf [] = []
      firstTokOnLine = fmap tokT $ listToMaybe $
          dropWhile ((solPnt >) . tokBegin) $
          takeWhile ((eolPnt >) . tokBegin) $ -- for laziness.
          filter (not . isErrorTok . tokT) $ allToks e
      shiftBlock = case firstTokOnLine of
        Just (Reserved t) | t `elem` [Where, Deriving] -> indentLevel
        Just (ReservedOp Haskell.Pipe) -> indentLevel
        Just (ReservedOp Haskell.Equal) -> indentLevel
        _ -> 0
      deepInGroup = maybe True insideGroup firstTokOnLine
      groupIndent (Tok {tokT = Special openChar, tokPosn = Posn _ _ openCol}) ctnt
          | deepInGroup = case firstTokNotOnLine ctnt of
              -- examine the first token of the group (but not on the line we are indenting!)
              Nothing -> openCol + nominalIndent openChar -- no such token: indent normally.
              Just t -> posnCol . tokPosn $ t -- indent along that other token
          | otherwise = openCol
      groupIndent (Tok _ _ _) _ = error "unable to indent code"
  case getLastPath [e] solPnt of
    Nothing -> return ()
    Just path -> let stops = stopsOf path
                 in trace ("Stops = " ++ show stops) $
                    trace ("firstTokOnLine = " ++ show firstTokOnLine) $
                    cycleIndentsB behaviour stops

cleverAutoIndentHaskellC :: Exp TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellC e behaviour = do
  indentSettings <- indentSettingsB
  let indentLevel = shiftWidth indentSettings
  previousIndent <- indentOfB =<< getNextNonBlankLineB Backward
  nextIndent     <- indentOfB =<< getNextNonBlankLineB Forward
  solPnt <- pointAt moveToSol
  eolPnt <- pointAt moveToEol
  let onThisLine ofs = ofs >= solPnt && ofs <= eolPnt
      firstTokNotOnLine = listToMaybe .
                              filter (not . onThisLine . posnOfs . tokPosn) .
                              filter (not . isErrorTok . tokT) . concatMap allToks
  let stopsOf :: [Hask.Exp TT] -> [Int]
      stopsOf (g@(Hask.Paren (Hask.PAtom open _) ctnt (Hask.PAtom close _)):ts)
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [groupIndent open ctnt]
            -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts
           -- this group is closed before this line; just skip it.
      stopsOf ((Hask.PAtom (Tok {tokT = t}) _):_) | startsLayout t || (t == ReservedOp Equal)
          = [nextIndent, previousIndent + indentLevel]
        -- of; where; etc. ends the previous line. We want to start the block here.
        -- Also use the next line's indent:
        -- maybe we are putting a new 1st statement in the block here.
      stopsOf (l@(Hask.PLet _ (Hask.Block _) _):ts') = [colOf' l | lineStartsWith (Reserved Haskell.In)] ++ stopsOf ts'
                                                       -- offer to align with let only if this is an "in"
      stopsOf (t@(Hask.Block _):ts') = [shiftBlock + colOf' t] ++ stopsOf ts'
                                       -- offer add another statement in the block
      stopsOf ((Hask.PGuard' (PAtom pipe  _) _ _):ts') = [tokCol pipe | lineStartsWith (ReservedOp Haskell.Pipe)] ++ stopsOf ts'
                                                                 -- offer to align against another guard
      stopsOf (d@(Hask.PData {}):ts') = colOf' d + indentLevel
                                           : stopsOf ts' --FIXME!
      stopsOf ((Hask.RHS (Hask.PAtom{}) (exp)):ts')
          = [(case firstTokOnLine of
              Just (Operator op) -> opLength op (colOf' exp) -- Usually operators are aligned against the '=' sign
              -- case of an operator should check so that value always is at least 1
              _ -> colOf' exp) | lineIsExpression ] ++ stopsOf ts'
                   -- offer to continue the RHS if this looks like an expression.
      stopsOf [] = [0] -- maybe it's new declaration in the module
      stopsOf (_:ts) = stopsOf ts -- by default, there is no reason to indent against an expression.
       -- calculate indentation of operator (must be at least 1 to be valid)
      opLength ts' r = let l = r - (length ts' + 1) -- I find this dubious...
                       in  if l > 0 then l else 1

      lineStartsWith tok = firstTokOnLine == Just tok
      lineIsExpression   = all (`notElem` [ReservedOp Haskell.Pipe, ReservedOp Haskell.Equal, ReservedOp RightArrow]) toksOnLine
                           && not (lineStartsWith (Reserved Haskell.In))
      -- TODO: check the tree instead of guessing by looking at tokens
      firstTokOnLine = listToMaybe toksOnLine
      toksOnLine = fmap tokT $
          dropWhile ((solPnt >) . tokBegin) $
          takeWhile ((eolPnt >) . tokBegin) $ -- for laziness.
          filter (not . isErrorTok . tokT) $ allToks e
      shiftBlock = case firstTokOnLine of
        Just (Reserved t) | t `elem` [Where, Deriving] -> indentLevel
        Just (ReservedOp Haskell.Pipe) -> indentLevel
        Just (ReservedOp Haskell.Equal) -> indentLevel
        _ -> 0
      deepInGroup = maybe True insideGroup firstTokOnLine
      groupIndent (Tok {tokT = Special openChar, tokPosn = Posn _ _ openCol}) ctnt
          | deepInGroup = case firstTokNotOnLine ctnt of
              -- examine the first token of the group
              -- (but not on the line we are indenting!)
              Nothing -> openCol + nominalIndent openChar
              -- no such token: indent normally.
              Just t -> posnCol . tokPosn $ t -- indent along that other token
          | otherwise = openCol
      groupIndent (Tok _ _ _) _ = error "unable to indent code"
  case getLastPath [e] solPnt of
    Nothing -> return ()
    Just path ->let stops = stopsOf path
                in trace ("Path = " ++ show path) $
                   trace ("Stops = " ++ show stops) $
                   trace ("Previous indent = " ++ show previousIndent) $
                   trace ("Next indent = " ++ show nextIndent) $
                   trace ("firstTokOnLine = " ++ show firstTokOnLine) $
                   cycleIndentsB behaviour stops

colOf' :: Foldable t => t TT -> Int
colOf' = maybe 0 tokCol . getFirstElement

tokCol :: Tok t -> Int
tokCol = posnCol . tokPosn


nominalIndent :: Char -> Int
nominalIndent '{' = 2
nominalIndent _ = 1


tokText :: Tok t -> BufferM String
tokText = readRegionB . tokRegion

isLineComment :: TT -> Bool
isLineComment = (Just Haskell.Line ==) . tokTyp . tokT

contiguous :: forall t. Tok t -> Tok t -> Bool
contiguous a b = lb - la <= 1
    where [la,lb] = fmap (posnLine . tokPosn) [a,b]

coalesce :: Tok Token -> Tok Token -> Bool
coalesce a b = isLineComment a && isLineComment b && contiguous a b

cleverPrettify :: [TT] -> BufferM ()
cleverPrettify toks = do
  pnt <- pointB
  let groups = groupBy' coalesce toks
      isCommentGroup g = (tokTyp $ tokT $ head $ g) `elem` fmap Just [Haskell.Line]
      thisCommentGroup = listToMaybe $ dropWhile ((pnt >) . tokEnd . last) $ filter isCommentGroup $ groups
                         -- FIXME: laziness
  case thisCommentGroup of
    Nothing -> return ()
    Just g -> do let region = mkRegion (tokBegin . head $ g) (tokEnd . last $ g)
                 text <- unwords . fmap (drop 2) <$> mapM tokText g
                 modifyRegionClever (const $ unlines' $ fmap ("-- " ++) $ fillText 80 $ text) region

tokTyp :: Token -> Maybe Haskell.CommentType
tokTyp (Comment t) = Just t
tokTyp _ = Nothing

-- TODO: export or remove
-- -- | Keyword-based auto-indenter for haskell.
-- autoIndentHaskellB :: IndentBehaviour -> BufferM ()
-- autoIndentHaskellB =
--   autoIndentWithKeywordsB [ "if"
--                           , "then"
--                           , "else"
--                           , "|"
--                           , "->"
--                           , "case" -- hmm
--                           , "in"
--                           -- Note tempted by having '=' in here that would
--                           -- potentially work well for 'data' declarations
--                           -- but I think '=' is so common in other places
--                           -- that it would introduce many spurious/annoying
--                           -- hints.
--                           ]
--                           [ "where"
--                           , "let"
--                           , "do"
--                           , "mdo"
--                           , "{-"
--                           , "{-|"
--                           , "--"
--                           ]
--
---------------------------
-- * Interaction with GHCi


newtype GhciBuffer = GhciBuffer {_ghciBuffer :: Maybe BufferRef}
    deriving (Default, Typeable, Binary)

instance YiVariable GhciBuffer
-- | Start GHCi in a buffer
ghci :: YiM BufferRef
ghci = do
    b <- GHCi.spawnProcess "ghci" []
    withEditor $ setDynamic $ GhciBuffer $ Just b
    return b

-- | Return GHCi's buffer; create it if necessary.
-- Show it in another window.
ghciGet :: YiM BufferRef
ghciGet = withOtherWindow $ do
    GhciBuffer mb <- withEditor $ getDynamic
    case mb of
        Nothing -> ghci
        Just b -> do
            stillExists <- withEditor $ isJust <$> findBuffer b
            if stillExists
                then do withEditor $ switchToBufferE b
                        return b
                else ghci

-- | Send a command to GHCi
ghciSend :: String -> YiM ()
ghciSend cmd = do
    b <- ghciGet
    withGivenBuffer b botB
    sendToProcess b (cmd ++ "\n")

-- | Load current buffer in GHCi
ghciLoadBuffer :: YiM ()
ghciLoadBuffer = do
    fwriteE
    f <- withBuffer (gets file)
    case f of
      Nothing -> error "Couldn't get buffer filename in ghciLoadBuffer"
      Just filename -> ghciSend $ ":load " ++ show filename

-- Tells ghci to infer the type of the identifier at point. Doesn't check for errors (yet)
ghciInferType :: YiM ()
ghciInferType = do
    nm <- withBuffer $ readUnitB unitWord
    when (not $ null nm) $
        withMinibufferGen nm noHint "Insert type of which identifier?"
        return (const $ return ()) ghciInferTypeOf

ghciInferTypeOf :: String -> YiM ()
ghciInferTypeOf nm = do
    buf <- ghciGet
    result <- Interactive.queryReply buf (":t " ++ nm)
    let successful = (not . null) nm &&and (zipWith (==) nm result)
    when successful $
         withBuffer $ moveToSol *> insertB '\n' *> leftB *> insertN result *> rightB
