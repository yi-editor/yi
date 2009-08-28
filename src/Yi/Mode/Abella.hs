-- Copyright (c) 2009 Nicolas Pouillard
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yi.Mode.Abella
  ( abellaModeVim, abellaModeEmacs, abella
  , abellaEval, abellaEvalFromProofPoint, abellaUndo, abellaGet, abellaSend)
where

import Prelude ()
import Control.Monad (replicateM_, join)
import Data.Char (isSpace)
import Data.Binary
import Data.Maybe (isJust)
import Data.List (isInfixOf)
import Yi.Core
import qualified Yi.Mode.Interactive as Interactive
import Yi.Modes (TokenBasedMode, linearSyntaxMode, anyExtension)
import qualified Yi.Lexer.Abella as Abella
import Yi.Syntax.Tree
import Yi.MiniBuffer (CommandArguments(..))
import Yi.Lexer.Alex
import Yi.Keymap.Vim (savingCommandY)

abellaModeGen :: (Char -> [Event]) -> TokenBasedMode Abella.Token
abellaModeGen abellaBinding =
  (linearSyntaxMode Abella.initState Abella.alexScanToken Abella.tokenToStyle)
  { modeName = "abella"
  , modeApplies = anyExtension ["thm"]
  , modeGetAnnotations = tokenBasedAnnots (sequenceA . tokToSpan . fmap Abella.tokenToText)
  , modeToggleCommentSelection = toggleCommentSelectionB "% " "%"
  , modeKeymap = topKeymapA ^: ((<||)
     (choice
      [ abellaBinding 'p' ?*>>! sav abellaUndo
      , abellaBinding 'e' ?*>>! sav abellaEval
      , abellaBinding 'n' ?*>>! sav abellaNext
      , abellaBinding 'a' ?*>>! sav abellaAbort
      , abellaBinding '\r' ?*>>! sav abellaEvalFromProofPoint
      ]))
  }
  where sav f = savingCommandY (flip replicateM_ f) 1

abellaModeVim :: TokenBasedMode Abella.Token
abellaModeVim = abellaModeGen (\ch -> [char '\\', char ch])

abellaModeEmacs :: TokenBasedMode Abella.Token
abellaModeEmacs = abellaModeGen (\ch -> [ctrlCh 'c', ctrlCh ch])

newtype AbellaBuffer = AbellaBuffer {_abellaBuffer :: Maybe BufferRef}
    deriving (Initializable, Typeable, Binary)

getProofPointMark :: BufferM Mark
getProofPointMark = getMarkB $ Just "p"

getTheoremPointMark :: BufferM Mark
getTheoremPointMark = getMarkB $ Just "t"

abellaEval :: YiM ()
abellaEval = do reg <- withBuffer . savingPointB $ do
                          join $ setMarkPointB <$> getProofPointMark <*> pointB
                          leftB
                          readRegionB =<< regionOfNonEmptyB unitSentence
                abellaSend reg

abellaEvalFromProofPoint :: YiM ()
abellaEvalFromProofPoint = abellaSend =<< withBuffer f
  where f = do mark <- getProofPointMark
               p <- getMarkPointB mark
               cur <- pointB
               setMarkPointB mark cur
               readRegionB $ mkRegion p cur

abellaNext :: YiM ()
abellaNext = do reg <- withBuffer $ rightB >> (readRegionB =<< regionOfNonEmptyB unitSentence)
                abellaSend reg
                withBuffer $ do moveB unitSentence Forward
                                rightB
                                untilB_ (not . isSpace <$> readB) rightB
                                untilB_ ((/= '%') <$> readB) $ moveToEol >> rightB >> firstNonSpaceB
                                join $ setMarkPointB <$> getProofPointMark <*> pointB

abellaUndo :: YiM ()
abellaUndo = do abellaSend "undo."
                withBuffer $ do moveB unitSentence Backward
                                join $ setMarkPointB <$> getProofPointMark <*> pointB

abellaAbort :: YiM ()
abellaAbort = do abellaSend "abort."
                 withBuffer $ do moveTo =<< getMarkPointB =<< getTheoremPointMark
                                 join $ setMarkPointB <$> getProofPointMark <*> pointB

-- | Start Abella in a buffer
abella :: CommandArguments -> YiM BufferRef
abella (CommandArguments args) = do
    b <- Interactive.interactive "abella" args
    withEditor . setDynamic . AbellaBuffer $ Just b
    return b

-- | Return Abella's buffer; create it if necessary.
-- Show it in another window.
abellaGet :: YiM BufferRef
abellaGet = withOtherWindow $ do
    AbellaBuffer mb <- withEditor $ getDynamic
    case mb of
        Nothing -> abella (CommandArguments [])
        Just b -> do
            stillExists <- withEditor $ isJust <$> findBuffer b
            if stillExists
                then do withEditor $ switchToBufferE b
                        return b
                else abella (CommandArguments [])

-- | Send a command to Abella
abellaSend :: String -> YiM ()
abellaSend cmd = do
    when ("Theorem" `isInfixOf` cmd) $
      withBuffer $ join $ setMarkPointB <$> getTheoremPointMark <*> pointB
    b <- abellaGet
    withGivenBuffer b botB
    sendToProcess b (cmd ++ "\n")
