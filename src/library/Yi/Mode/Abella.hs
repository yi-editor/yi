{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Abella
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 'Mode's and utility function for working with the Abella
-- interactive theorem prover.

module Yi.Mode.Abella
  ( abellaModeEmacs
  , abella
  , abellaEval
  , abellaEvalFromProofPoint
  , abellaUndo
  , abellaGet
  , abellaSend
  ) where

import           Control.Applicative (Applicative ((<*>)), (<$>))
import           Control.Lens        (assign, use, (%~), (&), (.=), (.~))
import           Control.Monad       (join, when)
import           Data.Binary         (Binary)
import           Data.Char           (isSpace)
import           Data.Default        (Default)
import           Data.Maybe          (isJust)
import qualified Data.Text           as T (isInfixOf, snoc, unpack)
import           Data.Typeable       (Typeable)
import           Yi.Buffer
import           Yi.Core             (sendToProcess)
import           Yi.Editor
import           Yi.Keymap           (YiM, topKeymapA)
import           Yi.Keymap.Keys      (Event, choice, ctrlCh, (<||), (?*>>!))
import qualified Yi.Lexer.Abella     as Abella (Token, lexer)
import           Yi.MiniBuffer       (CommandArguments (..))
import qualified Yi.Mode.Interactive as Interactive (spawnProcess)
import           Yi.Modes            (TokenBasedMode, anyExtension, styleMode)
import qualified Yi.Rope             as R (YiString, toText)
import           Yi.Types            (YiVariable)

abellaModeGen :: (Char -> [Event]) -> TokenBasedMode Abella.Token
abellaModeGen abellaBinding = styleMode Abella.lexer
  & modeNameA .~ "abella"
  & modeAppliesA .~ anyExtension ["thm"]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "%")
  & modeKeymapA .~ topKeymapA %~ (<||)
     (choice
      [ abellaBinding 'p' ?*>>! abellaUndo
      , abellaBinding 'e' ?*>>! abellaEval
      , abellaBinding 'n' ?*>>! abellaNext
      , abellaBinding 'a' ?*>>! abellaAbort
      , abellaBinding '\r' ?*>>! abellaEvalFromProofPoint
      ])

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
abellaEval = do
  reg <- withCurrentBuffer . savingPointB $ do
    join (assign . markPointA <$> getProofPointMark <*> pointB)
    leftB
    readRegionB =<< regionOfNonEmptyB unitSentence
  abellaSend reg

abellaEvalFromProofPoint :: YiM ()
abellaEvalFromProofPoint = abellaSend =<< withCurrentBuffer f
  where f = do mark <- getProofPointMark
               p <- use $ markPointA mark
               cur <- pointB
               markPointA mark .= cur
               readRegionB $ mkRegion p cur

abellaNext :: YiM ()
abellaNext = do
  reg <- withCurrentBuffer $ rightB >> (readRegionB =<< regionOfNonEmptyB unitSentence)
  abellaSend reg
  withCurrentBuffer $ do
    moveB unitSentence Forward
    rightB
    untilB_ (not . isSpace <$> readB) rightB
    untilB_ ((/= '%') <$> readB) $ moveToEol >> rightB >> firstNonSpaceB
    join (assign . markPointA <$> getProofPointMark <*> pointB)

abellaUndo :: YiM ()
abellaUndo = do
  abellaSend "undo."
  withCurrentBuffer $ do
    moveB unitSentence Backward
    join (assign . markPointA <$> getProofPointMark <*> pointB)

abellaAbort :: YiM ()
abellaAbort = do
  abellaSend "abort."
  withCurrentBuffer $ do
    moveTo =<< use . markPointA =<< getTheoremPointMark
    join (assign . markPointA <$> getProofPointMark <*> pointB)

-- | Start Abella in a buffer
abella :: CommandArguments -> YiM BufferRef
abella (CommandArguments args) = do
    b <- Interactive.spawnProcess "abella" (T.unpack <$> args)
    withEditor . putEditorDyn . AbellaBuffer $ Just b
    return b

-- | Return Abella's buffer; create it if necessary.
-- Show it in another window.
abellaGet :: YiM BufferRef
abellaGet = withOtherWindow $ do
    AbellaBuffer mb <- withEditor getEditorDyn
    case mb of
        Nothing -> abella (CommandArguments [])
        Just b -> do
            stillExists <- isJust <$> findBuffer b
            if stillExists
                then do withEditor $ switchToBufferE b
                        return b
                else abella (CommandArguments [])

-- | Send a command to Abella
abellaSend :: R.YiString -> YiM ()
abellaSend cmd' = do
  let cmd = R.toText cmd'
  when ("Theorem" `T.isInfixOf` cmd) $
    withCurrentBuffer $ join (assign . markPointA <$> getTheoremPointMark <*> pointB)
  b <- abellaGet
  withGivenBuffer b botB
  sendToProcess b . T.unpack $ cmd `T.snoc` '\n'
