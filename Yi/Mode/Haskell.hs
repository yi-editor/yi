-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Haskell-specific modes and commands.
module Yi.Mode.Haskell 
  (
   -- * Modes
   plainMode,
   cleverMode, 
   preciseMode,
   
   -- * Buffer-level operations
   haskellUnCommentSelectionB,
   haskellCommentSelectionB,
   haskellToggleCommentSelectionB,

   -- * IO-level operations
   ghciGet,
   ghciLoadBuffer
  ) where

import Data.Binary
import Data.List (isPrefixOf, dropWhile, takeWhile, filter, drop)
import Data.Maybe (maybe, listToMaybe, isJust)
import Data.Typeable
import Prelude (unwords)
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Indent
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Core
import Yi.Dynamic
import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Lexer.Alex (Tok(..),Posn(..),tokBegin,tokEnd,tokRegion)
import Yi.Lexer.Haskell (Token(..), ReservedType(..), startsLayout)
import Yi.Prelude
import Yi.Region
import Yi.String
import Yi.Syntax
import Yi.Syntax.Haskell as Hask
import Yi.Syntax.Paren as Paren
import Yi.Syntax.Tree
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import Yi.Lexer.Haskell as Haskell
import qualified Yi.Syntax.Linear as Linear
import qualified Yi.Mode.Interactive as Interactive
import Yi.Modes (anyExtension)

-- | Plain haskell mode, providing only list of stuff.
plainMode :: Mode (Linear.Result (Tok Token))
plainMode = emptyMode 
   {
     {-    
     Some of these are a little questionably haskell
     related. For example ".x" is an alex lexer specification
     I dare say that there are other file types that use ".x"
     as the file extension.
     For now though this is probably okay given the users of
     'yi' are mostly haskell hackers, as of yet. -}
     modeApplies = anyExtension ["hs", "x", "hsc", "hsinc"],
     modeName = "plain haskell",
     modeHL = ExtHL $
     mkHighlighter (Linear.incrScanner . haskellLexer) (\begin end pos -> Linear.getStrokes begin end pos . fmap Paren.tokenToStroke)
   , modeIndent = \_ast -> autoIndentHaskellB
   }

-- | "Clever" hasell mode, using the 
cleverMode :: Mode (Expr (Tok Haskell.Token))
cleverMode = emptyMode 
  {
    modeName = "haskell",
    modeApplies = modeApplies plainMode,
    modeIndent = cleverAutoIndentHaskellB,
    modeHL = ExtHL $
    mkHighlighter (IncrParser.scanner Paren.parse . Paren.indentScanner . haskellLexer)
      (\point begin end t -> Paren.getStrokes point begin end t)

  , modeAdjustBlock = adjustBlock
  , modePrettify = cleverPrettify
 }

-- | "Clever" hasell mode, using the 
preciseMode :: Mode (Hask.Tree TT)
preciseMode = emptyMode 
  {
    modeName = "precise haskell",
    modeApplies = modeApplies plainMode,
    --    modeIndent = cleverAutoIndentHaskellB,
    modeHL = ExtHL $
      mkHighlighter (IncrParser.scanner Hask.parse . Hask.indentScanner . haskellLexer)
        (\point begin end t -> Hask.getStrokes point begin end t)
    --}                              
    -- ,  modeAdjustBlock = adjustBlock
  -- , modePrettify = cleverPrettify
 }


haskellLexer = Alex.lexScanner Haskell.alexScanToken Haskell.initState 

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
                   forM_ [1..height] $ \_ -> do
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

-- TODO: use the indentSettings
indentLevel :: Int
indentLevel = 4

-- | Returns true if the token should be indented to look as "inside" the group.
insideGroup :: Token -> Bool
insideGroup (Special c) = not $ c `elem` "',;})]" 
insideGroup _ = True

cleverAutoIndentHaskellB :: Expr TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellB e behaviour = do
  previousIndent <- indentOfB =<< getNextNonBlankLineB Backward
  nextIndent     <- indentOfB =<< getNextNonBlankLineB Forward
  solPnt <- pointAt moveToSol
  eolPnt <- pointAt moveToEol
  let onThisLine ofs = ofs >= solPnt && ofs <= eolPnt
      firstTokNotOnLine = listToMaybe .
                              filter (not . onThisLine . posnOfs . tokPosn) .
                              filter (not . isErrorTok . tokT) . allToks 
  let stopsOf :: [Paren.Tree TT] -> [Int]
      stopsOf (g@(Paren.Paren open ctnt close):ts) 
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [groupIndent open ctnt]  -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts -- this group is closed before this line; just skip it.
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
  case getLastPath e solPnt of
    Nothing -> return ()
    Just path -> let stops = stopsOf path
                 in trace ("Stops = " ++ show stops) $      
                    trace ("firstTokOnLine = " ++ show firstTokOnLine) $      
                    cycleIndentsB behaviour stops

nominalIndent :: Char -> Int
nominalIndent '{' = 2
nominalIndent _ = 1


allToks :: Expr TT -> [TT]
allToks = concatMap toList

tokText :: Tok t -> BufferM String
tokText = readRegionB . tokRegion

isLineComment = (Just Haskell.Line ==) . tokTyp . tokT

contiguous :: forall t. Tok t -> Tok t -> Bool
contiguous a b = lb - la <= 1
    where [la,lb] = fmap (posnLine . tokPosn) [a,b]

coalesce :: Tok Token -> Tok Token -> Bool
coalesce a b = isLineComment a && isLineComment b && contiguous a b

cleverPrettify :: Expr TT -> BufferM ()
cleverPrettify e = do
  pnt <- pointB
  let toks = allToks e
      groups = groupBy' coalesce toks
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


-- | Comments the region using haskell line comments
haskellCommentSelectionB :: BufferM ()
haskellCommentSelectionB = linePrefixSelectionB "-- "

-- | uncomments a region of haskell line commented code
haskellUnCommentSelectionB :: BufferM ()
haskellUnCommentSelectionB = unLineCommentSelectionB "-- "

haskellToggleCommentSelectionB :: BufferM ()
haskellToggleCommentSelectionB = do
  l <- readUnitB Yi.Buffer.Normal.Line
  if ("--" `isPrefixOf` l)
    then haskellUnCommentSelectionB
    else haskellCommentSelectionB



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

-- | Return GHCi's buffer; create it if necessary
ghciGet :: YiM BufferRef
ghciGet = do
    GhciBuffer mb <- withEditor $ getDynamic
    case mb of
        Nothing -> ghci
        Just b -> do
            stillExists <- withEditor $ isJust <$> findBuffer b
            if stillExists 
                then return b
                else ghci
    
-- | Send a command to GHCi
ghciSend :: String -> YiM ()
ghciSend cmd = do
    b <- ghciGet
    sendToProcess b (cmd ++ "\n")
    
-- | Load current buffer in GHCi
ghciLoadBuffer :: YiM ()
ghciLoadBuffer = do
    fwriteE
    Just filename <- withBuffer $ getA fileA
    ghciSend $ ":load " ++ filename

