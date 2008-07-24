module Yi.Mode.Haskell (haskellMode, cleverHaskellMode, 
                        haskellUnCommentSelectionB, haskellCommentSelectionB, haskellToggleCommentSelectionB)  where

import Prelude (unwords)
import Yi.Prelude
import Data.Maybe (maybe, listToMaybe)
import Data.List (isPrefixOf, dropWhile, takeWhile, filter, groupBy, drop)
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Indent
import Yi.Prelude
import Yi.Syntax
import Yi.String
import Yi.Region
import Yi.Lexer.Alex (Tok(..),Posn(..),tokBegin,tokEnd,tokRegion)
import Yi.Lexer.Haskell (Token(..), ReservedType(..), startsLayout)
import qualified Yi.Syntax.Linear as Linear
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Haskell as Haskell
import Yi.Syntax.Paren as Paren
import Control.Applicative

haskellMode :: Mode (Linear.Result (Tok Token))
haskellMode = emptyMode 
   {
    modeHL = ExtHL $
    mkHighlighter (Linear.incrScanner . haskellLexer) (\begin end pos -> Linear.getStrokes begin end pos . fmap tokenToStroke)
   , modeIndent = \_ast -> autoIndentHaskellB
   }

cleverHaskellMode :: Mode (Expr (Tok Haskell.Token))
cleverHaskellMode = emptyMode {
    modeIndent = cleverAutoIndentHaskellB,
    modeHL = ExtHL $
{--    lexer `withScanner` IncrParser.mkHighlighter Fractal.parse
      (\begin end -> fmap tokenToStroke . Fractal.getStrokes begin end) id -}
--}

{--
    lexer `withScanner` IncrParser.mkHighlighter Paren.parse
      (\begin end t -> Paren.getStrokes begin end t) id
--}

--
    mkHighlighter (IncrParser.scanner parse . indentScanner . haskellLexer)
      (\point begin end t -> Paren.getStrokes point begin end t)
--}                              
  , modeAdjustBlock = adjustBlock
  , modePrettify = cleverPrettify
 }

haskellLexer = Alex.lexScanner Haskell.alexScanToken Haskell.initState 


adjustBlock :: Expr (Tok Token) -> Int -> BufferM ()
adjustBlock e len = do
  p <- pointB
  l <- curLn
  let t = getIndentingSubtree e p l
  case t of
    Nothing -> return ()
    Just it -> 
           savingExcursionB $ do
                   let (_startOfs, height) = getSubtreeSpan it
                   forM_ [1..height] $ \_ -> do
                               lineDown
                               if len >= 0 
                                 then do insertN (replicate len ' ') 
                                         leftN len
                                 else do
                                    deleteN 1
indentLevel :: Int
indentLevel = 4

insideGroup :: Token -> Bool
insideGroup (Special c) = not $ c `elem` "',;})" 
insideGroup _ = True

cleverAutoIndentHaskellB :: Expr TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellB e behaviour = do
  previousLine   <- getNextNonBlankLineB Backward
  previousIndent <- indentOfB previousLine
  solPnt <- pointAt moveToSol
  eolPnt <- pointAt moveToEol
  let stopsOf (g@(Group open _ close):ts) 
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [shiftGroup + (posnCol . tokPosn $ open)]  -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts -- this one is closed on before this line; just skip it.
      stopsOf ((Atom (Tok {tokT = t})):_) | startsLayout t = [previousIndent + indentLevel]
      stopsOf ((Atom _):ts) = stopsOf ts
         -- of; where; etc. we want to start the block here.
      stopsOf (t@(Stmt _):ts) = shiftBlock + maybe 0 (posnCol . tokPosn) (getFirstToken t) : stopsOf ts
      stopsOf (Error _:ts) = stopsOf ts
      stopsOf [] = []
      firstTokOnLine = fmap tokT $ listToMaybe $ 
          dropWhile ((solPnt >) . tokBegin) $ 
          takeWhile ((eolPnt >) . tokBegin) $ -- for laziness.
          filter (not . isErrorTok . tokT) $ allToks e
      shiftBlock = case firstTokOnLine of
        Just (Reserved t) | t `elem` [Where, Deriving] -> 4
        _ -> 0
      deepInGroup = maybe True insideGroup firstTokOnLine
      shiftGroup = if deepInGroup then 1 else 0
                     

        
  case getLastPath e solPnt of
    Nothing -> return ()
    Just path -> let stops = stopsOf path
                 in trace ("Stops = " ++ show stops) $      
                    trace ("firstTokOnLine = " ++ show firstTokOnLine) $      
                    cycleIndentsB behaviour stops
         
allToks :: Expr TT -> [TT]
allToks = concatMap toList

tokText :: Tok t -> BufferM String
tokText = readRegionB . tokRegion

cleverPrettify :: Expr TT -> BufferM ()
cleverPrettify e = do
  pnt <- pointB
  let toks = allToks e
      groups = groupBy ((==) `on` (tokTyp . tokT)) toks
      isCommentGroup g = (tokTyp $ tokT $ head $ g) `elem` fmap Just [Haskell.Line] --  Haskell.Text]
      thisCommentGroup = listToMaybe $ dropWhile ((pnt >) . tokEnd . last) $ filter isCommentGroup $ groups
                         -- FIXME: laziness
  case thisCommentGroup of
    Nothing -> return ()
    Just g -> do let region = mkRegion (tokBegin . head $ g) (tokEnd . last $ g)
                 text <- unwords . fmap (drop 2) <$> mapM tokText g
                 modifyRegionB (const $ unlines' $ fmap ("-- " ++) $ fillText 80 $ text) region
                 
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
  l <- readUnitB Line
  if ("--" `isPrefixOf` l)
    then haskellUnCommentSelectionB
    else haskellCommentSelectionB



