module Yi.Mode.Interactive where

import Prelude ()
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.Core
import Yi.Core
import Yi.Dired
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Prelude
import Yi.Region
import Yi.Style
import Yi.Syntax
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Compilation         as Compilation
import qualified Yi.Mode.Compilation         as Compilation
import qualified Yi.Syntax.Linear as Linear

atLastLine = savingPointB $ do
    moveToEol
    (==) <$> sizeB <*> pointB

mode :: Mode (Linear.Result (Tok Compilation.Token))
mode = Compilation.mode
  { 
   modeKeymap = (<||) (spec KEnter ?>>! do
       atEof <- withBuffer $ atLastLine
       if atEof
         then feedCommand
         else withSyntax modeFollow)
  }

feedCommand = do
    b <- withEditor $ getBuffer
    cmd <- withBuffer $ do 
        botB
        insertN "\n"
        m <- getMarkB (Just "Prompt")
        p <- pointB
        q <- getMarkPointB m
        cmd <- readRegionB $ mkRegion p q
        setMarkPointB m p
        return cmd
    sendToProcess b cmd

