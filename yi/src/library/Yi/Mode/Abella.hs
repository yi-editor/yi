-- Copyright (c) 2009 Nicolas Pouillard
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yi.Mode.Abella
  ( abellaModeEmacs, abella
  , abellaEval, abellaEvalFromProofPoint, abellaUndo, abellaGet, abellaSend)
where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Char (isSpace)
import Data.Binary
import Data.Maybe (isJust)
import Data.List (isInfixOf)
import Data.Default
import Data.Typeable
import Data.Traversable (sequenceA)
import Yi.Core
import qualified Yi.Mode.Interactive as Interactive
import Yi.Modes (TokenBasedMode, linearSyntaxMode, anyExtension)
import qualified Yi.Lexer.Abella as Abella
import Yi.Syntax.Tree
import Yi.MiniBuffer (CommandArguments(..))
import Yi.Lexer.Alex

abellaModeGen :: (Char -> [Event]) -> TokenBasedMode Abella.Token
abellaModeGen abellaBinding =
  (linearSyntaxMode Abella.initState Abella.alexScanToken Abella.tokenToStyle)
  { modeName = "abella"
  , modeApplies = anyExtension ["thm"]
  , modeGetAnnotations = tokenBasedAnnots (sequenceA . tokToSpan . fmap Abella.tokenToText)
  , modeToggleCommentSelection = toggleCommentSelectionB "% " "%"
  , modeKeymap = topKeymapA %~ (<||)
     (choice
      [ abellaBinding 'p' ?*>>! abellaUndo
      , abellaBinding 'e' ?*>>! abellaEval
      , abellaBinding 'n' ?*>>! abellaNext
      , abellaBinding 'a' ?*>>! abellaAbort
      , abellaBinding '\r' ?*>>! abellaEvalFromProofPoint
      ])
  }

abellaModeEmacs :: TokenBasedMode Abella.Token
abellaModeEmacs = abellaModeGen (\ch -> [ctrlCh 'c', ctrlCh ch])

newtype AbellaBuffer = AbellaBuffer {_abellaBuffer :: Maybe BufferRef}
    deriving (Default, Typeable, Binary)
instance YiVariable AbellaBuffer

getProofPointMark :: BufferM Mark
getProofPointMark = getMarkB $ Just "p"

getTheoremPointMark :: BufferM Mark
getTheoremPointMark = getMarkB $ Just "t"

abellaEval :: YiM ()
abellaEval = do reg <- withBuffer . savingPointB $ do
                          join (assign . markPointA <$> getProofPointMark <*> pointB)
                          leftB
                          readRegionB =<< regionOfNonEmptyB unitSentence
                abellaSend reg

abellaEvalFromProofPoint :: YiM ()
abellaEvalFromProofPoint = abellaSend =<< withBuffer f
  where f = do mark <- getProofPointMark
               p <- use $ markPointA mark
               cur <- pointB
               markPointA mark .= cur
               readRegionB $ mkRegion p cur

abellaNext :: YiM ()
abellaNext = do reg <- withBuffer $ rightB >> (readRegionB =<< regionOfNonEmptyB unitSentence)
                abellaSend reg
                withBuffer $ do moveB unitSentence Forward
                                rightB
                                untilB_ (not . isSpace <$> readB) rightB
                                untilB_ ((/= '%') <$> readB) $ moveToEol >> rightB >> firstNonSpaceB
                                join (assign . markPointA <$> getProofPointMark <*> pointB)

abellaUndo :: YiM ()
abellaUndo = do abellaSend "undo."
                withBuffer $ do moveB unitSentence Backward
                                join (assign . markPointA <$> getProofPointMark <*> pointB)

abellaAbort :: YiM ()
abellaAbort = do abellaSend "abort."
                 withBuffer $ do moveTo =<< use . markPointA =<< getTheoremPointMark
                                 join (assign . markPointA <$> getProofPointMark <*> pointB)

-- | Start Abella in a buffer
abella :: CommandArguments -> YiM BufferRef
abella (CommandArguments args) = do
    b <- Interactive.spawnProcess "abella" args
    withEditor . setDynamic . AbellaBuffer $ Just b
    return b

-- | Return Abella's buffer; create it if necessary.
-- Show it in another window.
abellaGet :: YiM BufferRef
abellaGet = withOtherWindow $ do
    AbellaBuffer mb <- withEditor getDynamic
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
      withBuffer $ join (assign . markPointA <$> getTheoremPointMark <*> pointB)
    b <- abellaGet
    withGivenBuffer b botB
    sendToProcess b (cmd ++ "\n")
