module Yi.Mode.Haskell (haskellMode, cleverHaskellMode, 
                        haskellUnCommentSelectionB, haskellCommentSelectionB)  where

import Prelude ()
import Data.Maybe (maybe)
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Indent
import Yi.Prelude
import Yi.Syntax
import Yi.Syntax.Alex (Tok(..),Posn(..))
import Yi.Syntax.Haskell (Token)
import Yi.Syntax.Paren
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Syntax.Alex as Alex
import qualified Yi.Syntax.Haskell             as Haskell
import qualified Yi.Syntax.Paren as Paren
import Control.Arrow (first)

haskellMode :: Mode Alex.Result
haskellMode = emptyMode 
   {
    modeHL = ExtHL $
    Alex.mkHighlighter Haskell.initState (fmap (first tokenToStroke) . Haskell.alexScanToken)
   , modeIndent = \_ast -> autoIndentHaskellB

   }

cleverHaskellMode :: Mode (Expr (Tok Haskell.Token))
cleverHaskellMode = haskellMode {
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
    (indentScanner . lexer) `withScanner` IncrParser.mkHighlighter parse
      (\point begin end t -> getStrokes point begin end t) snd
--}                              
  , modeAdjustBlock = adjustBlock
}
    where lexer = Alex.lexScanner Haskell.alexScanToken Haskell.initState 



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



cleverAutoIndentHaskellB :: Expr TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellB e behaviour = do
  previousLine   <- getNextNonBlankLineB Backward
  previousIndent <- indentOfB previousLine
  solPnt <- savingPointB (moveToSol >> pointB)
  let stopsOf (g@(Group open _ close):ts) 
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [1 + (posnCol . tokPosn $ open)]  -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts -- this one is closed on before this line; just skip it.
      stopsOf ((Atom (Tok {tokT = Haskell.IndentReserved})):ts) = [previousIndent + 2]
      stopsOf ((Atom _):ts) = stopsOf ts
         -- of; where; etc. we want to start the block here.
      stopsOf (t:ts) = maybe 0 (posnCol . tokPosn) (getFirstToken t) : stopsOf ts
      stopsOf [] = []
  case getLastPath e solPnt of
    Nothing -> return ()
    Just path -> let stops = stopsOf path
                 in trace ("Stops = " ++ show stops) $      
                    cycleIndentsB behaviour stops
         

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
haskellCommentSelectionB = lineCommentSelectionB "-- "

-- | uncomments a region of haskell line commented code
haskellUnCommentSelectionB :: BufferM ()
haskellUnCommentSelectionB = unLineCommentSelectionB "-- "

