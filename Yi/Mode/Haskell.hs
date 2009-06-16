{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Haskell-specific modes and commands.
module Yi.Mode.Haskell 
  (
   -- * Modes
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

import Data.Binary
import Data.List (dropWhile, takeWhile, filter, drop, length)
import Data.Maybe (maybe, listToMaybe, isJust, catMaybes)
import Prelude (unwords, zipWith)
import Yi.Core
import Yi.File
import Yi.Lexer.Alex (Tok(..),Posn(..),tokBegin,tokEnd,tokRegion)
import Yi.Lexer.Haskell (Token(..), ReservedType(..), startsLayout)
import Yi.Prelude
import Yi.Region
import Yi.String
import Yi.Syntax
import Yi.Syntax.Haskell as Hask
import Yi.Syntax.Strokes.Haskell as HS
import Yi.Syntax.Paren as Paren
import Yi.Syntax.Tree
import Yi.Syntax.OnlineTree as OnlineTree
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.LiterateHaskell as LiterateHaskell
import Yi.Lexer.Haskell as Haskell
import qualified Yi.Mode.Interactive as Interactive
import Yi.Modes (anyExtension, extensionOrContentsMatch)
import Yi.Mode.Haskell.Dollarify
import Yi.MiniBuffer

haskellAbstract :: Mode syntax
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
cleverMode :: Mode (Expr (Tok Haskell.Token))
cleverMode = haskellAbstract
  {
    modeIndent = cleverAutoIndentHaskellB,
    modeGetStrokes = \t point begin end -> Paren.getStrokes point begin end t,
    modeHL = ExtHL $
    mkHighlighter (skipScanner 50 . IncrParser.scanner Paren.parse . Paren.indentScanner . haskellLexer)

  , modeAdjustBlock = adjustBlock
  , modePrettify = (cleverPrettify . allToks)
  , modeGetAnnotations = tokenBasedAnnots Paren.tokenToAnnot

 }

fastMode :: Mode (OnlineTree.TreeAtPos TT)
fastMode = haskellAbstract
  {
    modeName = "fast haskell",
    modeHL = ExtHL $
    mkHighlighter (IncrParser.scanner OnlineTree.manyToks . haskellLexer),
    modeGetStrokes = tokenBasedStrokes Paren.tokenToStroke,
    modeGetAnnotations = tokenBasedAnnots Paren.tokenToAnnot
 }

literateMode :: Mode [Paren.Tree TT]
literateMode = haskellAbstract
  { modeName = "literate haskell"
  , modeApplies = anyExtension ["lhs"]
  , modeHL = ExtHL $
    mkHighlighter (IncrParser.scanner Paren.parse . Paren.indentScanner . literateHaskellLexer)
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
      mkHighlighter (IncrParser.scanner Hask.parse . Hask.indentScanner . haskellLexer)
--   , modePrettify = cleverPrettify . allToks . getExprs
--   , modeGetAnnotations = (tokenBasedAnnots Hask.tokenToAnnot) . getExprs
 }


haskellLexer :: Scanner Point Char -> Scanner (Alex.AlexState Haskell.HlState) (Tok Token) 
haskellLexer = Alex.lexScanner Haskell.alexScanToken Haskell.initState 

literateHaskellLexer :: Scanner Point Char -> Scanner (Alex.AlexState LiterateHaskell.HlState) (Tok Token)
literateHaskellLexer = Alex.lexScanner LiterateHaskell.alexScanToken LiterateHaskell.initState

adjustBlock :: Expr (Tok Token) -> Int -> BufferM ()
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

cleverAutoIndentHaskellB :: Expr TT -> IndentBehaviour -> BufferM ()
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
                              filter (not . isErrorTok . tokT) . allToks 
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
      stopsOf (Paren.Error _:ts) = stopsOf ts
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
  case getLastPath e solPnt of
    Nothing -> return ()
    Just path -> let stops = stopsOf path
                 in trace ("Stops = " ++ show stops) $      
                    trace ("firstTokOnLine = " ++ show firstTokOnLine) $      
                    cycleIndentsB behaviour stops

cleverAutoIndentHaskellC :: Hask.Tree TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellC (Program _ (Just prog)) beh
    = cleverAutoIndentHaskellC' (getExprs prog) beh
cleverAutoIndentHaskellC (Program _ (Nothing)) _ = return ()
cleverAutoIndentHaskellC' ::[Exp TT] -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellC' e behaviour = do
  indentSettings <- indentSettingsB
  let indentLevel = shiftWidth indentSettings
  previousIndent <- indentOfB =<< getNextNonBlankLineB Backward
  nextIndent     <- indentOfB =<< getNextNonBlankLineB Forward
  solPnt <- pointAt moveToSol
  eolPnt <- pointAt moveToEol
  let onThisLine ofs = ofs >= solPnt && ofs <= eolPnt
      firstTokNotOnLine = listToMaybe .
                              filter (not . onThisLine . posnOfs . tokPosn) .
                              filter (not . isErrorTok . tokT) . allToks
  let stopsOf :: [Hask.Exp TT] -> [Int]
      stopsOf (g@(Hask.Paren (Hask.PAtom open _) ctnt (Hask.PAtom close _)):ts)
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [groupIndent open ctnt]  -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts -- this group is closed before this line; just skip it.
      stopsOf ((Hask.PAtom (Tok {tokT = t}) _):_) | startsLayout t = [nextIndent, previousIndent + indentLevel]
        -- of; where; etc. we want to start the block here.
        -- Also use the next line's indent:
        -- maybe we are putting a new 1st statement in the block here.
      stopsOf ((Hask.PAtom _ __):ts) = stopsOf ts
      stopsOf ((Hask.PWhere (Hask.PAtom w _) _):_) = case firstTokOnLine of
         Nothing ->  0 : (firstTokOnCol w + 6) : []
         Just _ -> 0 : firstTokOnCol w + 6 : []
         -- any random part of expression, we ignore it.
      stopsOf (l@(Hask.PLet le e@(Hask.Block expr) _):ts') =
         case firstTokOnLine of
             Just (Reserved In) -> colOf l : []
             Just (Reserved Let) -> [0] -- TODO
             _ -> colOf l : stopsOf ts'
      stopsOf (t@(Hask.Block _):ts') = shiftBlock + maybe 0 firstTokOnCol (getFirstElement t) : stopsOf ts'
      stopsOf ((Hask.PGuard' pipe _ _ (e:expr)):ts') = case firstTokOnLine of
          Nothing -> maybe 0 firstTokOnCol (getFirstElement e) : stopsOf expr ++ stopsOf ts'
          Just (ReservedOp Haskell.Pipe) -> firstTokOnCol pipe : []
          _ -> maybe 0 firstTokOnCol (getFirstElement e) : stopsOf expr
      stopsOf (Hask.PError _ _ _:ts') = stopsOf ts'
      stopsOf (d@(Hask.PData _ _ _ _):ts') = colOf d + indentLevel : stopsOf ts'
      stopsOf ((Hask.RHS (Hask.PAtom eq _) []):ts') = previousIndent : (firstTokOnCol eq + 2) : stopsOf ts'
      stopsOf ((Hask.RHS (Hask.PAtom eq _) r@(exp:_)):ts') = case firstTokOnLine of
          Nothing -> previousIndent : maybe 0 firstTokOnCol (getFirstElement exp) : stopsOf r ++ stopsOf ts'
          Just (ReservedOp Haskell.Equal) -> firstTokOnCol eq : stopsOf r
          Just (Reserved Haskell.Where) -> fmap (+indentLevel) (stopsOf ts')
          Just (Operator op) -> opLength op (maybe 0 firstTokOnCol (getFirstElement exp)) : stopsOf r
          -- case of an operator should check so that value always is at least 1
          Just _ -> previousIndent : maybe 0 firstTokOnCol (getFirstElement exp) : stopsOf r ++ stopsOf ts'
      stopsOf ((Hask.Expr e):ts) = stopsOf e ++ stopsOf ts
      stopsOf ((Hask.TS _ _):ts') = stopsOf ts'
      stopsOf [] = []
      stopsOf (r:_) = error (show r) -- stopsOf ts -- not yet handled stuff
       -- calculate indentation of operator (must be at least 1 to be valid)
      colOf :: Foldable t => t TT -> Int
      colOf = maybe 0 firstTokOnCol . getFirstElement
      opLength ts' r = let l = r - (length ts' + 1)
                       in  if l > 0 then l else 1
      firstTokOnCol = posnCol . tokPosn
      firstTokOnLine = fmap tokT $ listToMaybe $
          dropWhile ((solPnt >) . tokBegin) $
          takeWhile ((eolPnt >) . tokBegin) $ -- for laziness.
          filter (not . isErrorTok . tokT) $ allToks e
      shiftBlock = case firstTokOnLine of
        Just (Reserved t) | t `elem` [Let,In,Where, Deriving] -> indentLevel -- no sure let/in should be here
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
  case getLastPath e solPnt of
    Nothing -> return ()
    Just path ->let stops = stopsOf path
                in trace ("Stops = " ++ show stops) $
                   trace ("firstTokOnLine = " ++ show firstTokOnLine) $
                   cycleIndentsB behaviour stops

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

-- | Keyword-based auto-indenter for haskell.
autoIndentHaskellB :: IndentBehaviour -> BufferM ()
autoIndentHaskellB =
  autoIndentWithKeywordsB [ "if"
                          , "then"
                          , "else"
                          , "|"
                          , "->"
                          , "case" -- hmm
                          , "in"
                          -- Note tempted by having '=' in here that would
                          -- potentially work well for 'data' declarations
                          -- but I think '=' is so common in other places
                          -- that it would introduce many spurious/annoying
                          -- hints.
                          ]
                          [ "where"
                          , "let"
                          , "do"
                          , "mdo"
                          , "{-"
                          , "{-|"
                          , "--"
                          ]

---------------------------
-- * Interaction with GHCi


newtype GhciBuffer = GhciBuffer {_ghciBuffer :: Maybe BufferRef}
    deriving (Initializable, Typeable, Binary)

-- | Start GHCi in a buffer
ghci :: YiM BufferRef
ghci = do 
    b <- Interactive.interactive "ghci" []
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
    Just filename <- withBuffer $ gets file
    ghciSend $ ":load " ++ filename



-- Tells ghci to infer the type of the identifier at point. Doesn't check for errors (yet)
ghciInferType :: YiM ()
ghciInferType = do
    name <- withBuffer $ readUnitB unitWord
    when (not $ null name) $ 
        withMinibufferGen name noHint "Insert type of which identifier?" return ghciInferTypeOf

ghciInferTypeOf :: String -> YiM ()
ghciInferTypeOf name = do
    buf <- ghciGet
    result <- Interactive.queryReply buf (":t " ++ name)
    let successful = (not . null) name &&and (zipWith (==) name result)
    when successful $
         withBuffer $ moveToSol *> insertB '\n' *> leftB *> insertN result *> rightB

