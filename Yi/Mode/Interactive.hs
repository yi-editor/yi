module Yi.Mode.Interactive where

import Prelude ()
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.Core
import Yi.Core
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Prelude
import Yi.Region
import Yi.Lexer.Alex (Tok(..))
import qualified Yi.Lexer.Compilation         as Compilation
import qualified Yi.Mode.Compilation         as Compilation
import qualified Yi.Syntax.Linear as Linear

atLastLine :: BufferM Bool
atLastLine = savingPointB $ do
    moveToEol
    (==) <$> sizeB <*> pointB

mode :: Mode (Linear.Result (Tok Compilation.Token))
mode = Compilation.mode
  { 
   modeKeymap = (<||) (spec KEnter ?>>! do
       eof <- withBuffer $ atLastLine
       if eof
         then feedCommand
         else withSyntax modeFollow)
  }

-- | Send the type command to the process
feedCommand :: YiM ()
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

