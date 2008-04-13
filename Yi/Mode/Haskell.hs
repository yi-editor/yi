module Yi.Mode.Haskell where

import Data.List (replicate)
import Prelude ()
import Yi.Buffer
import Yi.Indent
import Yi.Modes
import Yi.Prelude
import Yi.Syntax
import Yi.Syntax.Alex (Tok(..))
import Yi.Syntax.Haskell (Token)
import Yi.Syntax.Paren
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Syntax.Alex as Alex
import qualified Yi.Syntax.Haskell             as Haskell
import qualified Yi.Syntax.Paren as Paren

cleverHaskellMode :: Mode (Expr (Tok Haskell.Token))
cleverHaskellMode = fundamental {
    modeIndent = autoIndentHaskellB,
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




