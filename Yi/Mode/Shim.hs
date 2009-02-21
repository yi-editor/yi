{-# LANGUAGE DeriveDataTypeable #-}

module Yi.Mode.Shim where
    
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.List.PointedList.Circular as PL
import FastString
import Outputable hiding (char)
import Prelude ()
import Shim.Hsinfo as Hsinfo
import SrcLoc
import Yi
import Yi.Buffer
import Yi.GHC
import Yi.Prelude
import qualified Shim.Hsinfo as Hsinfo

jumpToSrcLoc :: SrcLoc -> YiM ()
jumpToSrcLoc locn = 
    if isGoodSrcLoc locn 
       then jumpToE (unpackFS $ srcLocFile locn) (srcLocLine locn) (srcLocCol locn)
       else msgEditor $ show (ppr locn defaultUserStyle)

jumpToNextNote :: YiM ()
jumpToNextNote = do
  note <- withEditor $ do
    modA notesA (fmap PL.next)
    getsA notesA (fmap PL.focus)
  case note of
    Nothing -> msgEditor "No note!"
    Just n -> jumpToSrcLoc $ srcSpanStart $ srcSpan $ n

-- | Position of the cursor
cursorPos :: BufferM (Maybe String, Int, Int)
cursorPos = (,,) <$> gets file <*> curLn <*> curCol

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
  lnReg <- withBuffer $ regionOfB Yi.Line
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
minorMode :: Mode syntax -> Mode syntax
minorMode m = m
   {
    modeName = modeName m ++ "+shim",
    modeKeymap = modeKeymap m . ((<||) 
      (((ctrl $ char 'c') ?>> choice
        [ctrl (char 't') ?>>! typeAtPos,
         char '!' ?>> char 't' ?>>! annotType,
         char '!' ?>> char '=' ?>>! annotType,
         ctrl (char 'd') ?>>! jumpToDefinition,
         ctrl (char 'l') ?>>! do
             withEditor $ do
                 withOtherWindow $ do
                     switchToBufferWithNameE "*messages*"
             msgEditor "Loading..."
             Just filename <- withBuffer $ gets file
             runShimThread (Hsinfo.load filename True Nothing >> return ())
             return ()
         ]
       ) <|> (ctrl (char 'x') ?>> char '`' ?>>! jumpToNextNote)))
   }

