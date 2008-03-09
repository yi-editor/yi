module Yi.Mode.Shim where
    
import Data.List
import Yi.Keymap.Emacs.Keys
import Yi.Yi
import Shim.Hsinfo as Hsinfo
import Yi.Buffer
import Yi.Modes
import Control.Monad.State

import qualified Shim.Hsinfo as Hsinfo
import Shim.SHM
import Shim.Sexp
import Shim.Utils

import SrcLoc
import ErrUtils ( Severity(..) )
import FastString
import Directory

import Control.Monad.State
import System.FilePath ( (</>) )


modeTable fname | ".hs" `isSuffixOf` fname = Just mode
modeTable _ = Nothing

mode = haskellMode
   {
    modeKeymap = rebind [
              ("C-c C-l", write $ do
                 msgEditor "Loading..."
                 Just filename <- withBuffer $ gets file
                 (res,_) <- withShim $ Hsinfo.load filename True Nothing
                 msgEditor "Result:"
                 msgEditor (show res)
                 withEditor $ withOtherWindow $ do
                   switchToBufferWithNameE "*messages*"
              )
             ]
   }

