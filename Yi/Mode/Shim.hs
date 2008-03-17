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

import SrcLoc
import ErrUtils ( Severity(..) )
import FastString

import Control.Monad.State
import Yi.WindowSet as Robin
import Outputable
import Yi.Accessor
import Data.Typeable
import Yi.Prelude
import Prelude ()
import Data.Char

modeTable :: ReaderT String Maybe Mode
modeTable = ReaderT $ \fname -> case () of 
                        _ | ".hs" `isSuffixOf` fname -> Just mode
                        _ ->  Nothing

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

-- | Position of the cursor
cursorPos :: BufferM (Maybe String, Int, Int)
cursorPos = (,,) <$> gets file <*> curLn <*> offsetFromSol

-- | Type of the symbol at current position
typeAtPos :: YiM String
typeAtPos = do
  (Just filename,line,col) <- withBuffer cursorPos
  withShim $ do
      Hsinfo.findTypeOfPos filename line col Nothing

-- | Annotate the current function with its type
annotType :: YiM ()
annotType = do
  t <- typeAtPos
  withBuffer $ do
     moveToSol
     insertN $ t ++ "\n"

-- | Update a line of the form "-- expr = value" with the new value of expr
annotValue :: YiM ()
annotValue = do
  lnReg <- withBuffer $ regionOfB Line
  Just sourcefile <- withBuffer $ gets file
  ln <- withBuffer $ readRegionB lnReg
  let lhs = takeWhile (/= '=') $ dropWhile (== '-') $ dropWhile isSpace $ ln
  newVal <- withShim $ do
               ses <- getSessionFor sourcefile
               evaluate ses lhs
  withBuffer $ replaceRegionB lnReg ("--" ++ lhs ++ "= " ++ newVal)

jumpToDefinition :: YiM ()
jumpToDefinition = do
  (Just filename,line,col) <- withBuffer cursorPos
  locn <- withShim $ Hsinfo.findDefinition filename line col Nothing
  jumpToSrcLoc locn

-- NOTE: source argument to Hsinfo functions can be used to provide
-- source text, apparently.
mode :: Mode
mode = haskellMode
   {
    modeKeymap = rebind [
              ("C-c C-t", write typeAtPos),
              ("C-c ! t", write annotType),
              ("C-c ! =", write annotValue),
              ("C-c C-d", write jumpToDefinition),
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

