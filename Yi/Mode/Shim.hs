{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

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
import Yi.WindowSet as Robin
import Outputable
import Yi.Accessor
import Data.Typeable
import Control.Arrow
modeTable fname | ".hs" `isSuffixOf` fname = Just mode
modeTable _ = Nothing

jumpToSrcLoc :: SrcLoc -> YiM ()
jumpToSrcLoc locn = 
    if isGoodSrcLoc locn 
       then jumpToE (unpackFS $ srcLocFile locn) (srcLocLine locn) (srcLocCol locn)
       else msgEditor $ show (ppr locn defaultUserStyle)


type T = (Maybe (Robin.WindowSet CompileNote))
newtype ShimNotes = ShimNotes { fromShimNotes :: T }
    deriving Typeable
instance Initializable ShimNotes where
    initial = ShimNotes Nothing



notesA :: Accessor Editor T
notesA =  (Accessor fromShimNotes (\f (ShimNotes x) -> ShimNotes (f x))) 
          .> dynamicValueA .> dynamicA 


jumpToNextNote :: YiM ()
jumpToNextNote = do
  note <- withEditor $ do
    modifyA notesA (fmap forward)
    getsA notesA (fmap current)
  case note of
    Nothing -> msgEditor "No note!"
    Just n -> jumpToSrcLoc $ srcSpanStart $ srcSpan $ n
    

mode = haskellMode
   {
    modeKeymap = rebind [
              ("C-c C-l", write $ do
                 msgEditor "Loading..."
                 Just filename <- withBuffer $ gets file
                 (res,_) <- withShim $ Hsinfo.load filename True Nothing
                 msgEditor "Result:"
                 msgEditor (show res)
                 withEditor $ do
                   setA notesA (Robin.fromList $ compResultNotes res)
                   withOtherWindow $ do
                     switchToBufferWithNameE "*messages*"
              ),
              ("C-x `", write jumpToNextNote)
             ]
   }

