-- | a mode for GHCi, implemented as tweaks on Interaction mode
module Yi.Mode.GHCi where

import Control.Lens
import Data.List (elemIndex)
import Yi.Core
import Yi.Lexer.Alex (Tok)
import Yi.Lexer.Compilation (Token())
import qualified Yi.Syntax.OnlineTree as OnlineTree

import qualified Yi.Mode.Interactive as I

mode :: Mode (OnlineTree.Tree (Tok Token))
mode = I.mode
  { modeName = "ghci",
    modeKeymap = (topKeymapA %~ (choice [spec KHome ?>>! homeKey] <||)) . modeKeymap I.mode
  }

-- | The GHCi prompt always begins with ">"; this goes to just before it, or if one is already at the start
-- of the prompt, goes to the beginning of the line. (If at the beginning of the line, this pushes you forward to it.)
homeKey :: BufferM ()
homeKey = do l <- readLnB
             let epos = elemIndex '>' l
             case epos of
                 Nothing -> moveToSol
                 Just pos -> do (_,mypos) <- getLineAndCol
                                if mypos == (pos+2) then moveToSol
                                 else moveToSol >> moveXorEol (pos+2)

spawnProcess :: FilePath -> [String] -> YiM BufferRef
spawnProcess = I.spawnProcessMode mode
