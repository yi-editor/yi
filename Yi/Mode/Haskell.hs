module Yi.Mode.Haskell (haskellMode, cleverHaskellMode, haskellUnCommentSelectionB)  where

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



cleverAutoIndentHaskellB :: Expr (Tok Token) -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellB e behaviour = do
  previousLine   <- getPreviousNonBlankLineB
  previousIndent <- indentOfB previousLine
  solPnt <- savingPointB (moveToSol >> pointB)
  let stopOf (Group open _ _) = 1 + (posnCol . tokPosn $ open)
      stopOf (Atom (Tok {tokT = Haskell.IndentReserved})) = previousIndent + 4
      stopOf t = maybe 0 (posnCol . tokPosn) (getFirstToken t)
  case getLastPath e solPnt of
    Nothing -> return ()
    Just path -> let stops = fmap stopOf path
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


-- | uncomments a region of haskell line commented code
haskellUnCommentSelectionB :: BufferM ()
haskellUnCommentSelectionB = unLineCommentSelectionB "-- "
