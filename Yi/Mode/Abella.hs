-- Copyright (c) 2009 Nicolas Pouillard
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yi.Mode.Abella
  (abellaModeVim, abellaModeEmacs, abella, abellaEval, abellaUndo, abellaGet, abellaSend)
where

import Prelude ()
import Data.Char (isSpace)
import Data.Binary
import Data.Maybe (isJust)
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
  , modeKeymap = (<||)
     (choice
      [ abellaBinding 'p' ?*>>! abellaUndo
      , abellaBinding 'e' ?*>>! abellaEval
      , abellaBinding 'n' ?*>>! abellaNext
      , abellaBinding 'a' ?*>>! abellaAbort
      ])
  }

abellaModeVim :: TokenBasedMode Abella.Token
abellaModeVim = abellaModeGen (\ch -> [char '\\', char ch])

abellaModeEmacs :: TokenBasedMode Abella.Token
abellaModeEmacs = abellaModeGen (\ch -> [ctrlCh 'c', ctrlCh ch])

newtype AbellaBuffer = AbellaBuffer {_abellaBuffer :: Maybe BufferRef}
    deriving (Initializable, Typeable, Binary)

abellaEval :: YiM ()
abellaEval = do reg <- withBuffer . savingPointB $ do
                          leftB
                          readRegionB =<< regionOfNonEmptyB unitSentence
                abellaSend reg

abellaNext :: YiM ()
abellaNext = do reg <- withBuffer $ readRegionB =<< regionOfNonEmptyB unitSentence
                abellaSend reg
                withBuffer $ do moveB unitSentence Forward
                                rightB
                                untilB_ (not . isSpace <$> readB) rightB
                                untilB_ ((/= '%') <$> readB) $ moveToEol >> rightB >> firstNonSpaceB

abellaUndo :: YiM ()
abellaUndo = do abellaSend "undo."
                withBuffer $ moveB unitSentence Backward

abellaAbort :: YiM ()
abellaAbort = abellaSend "abort."

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
    b <- abellaGet
    withGivenBuffer b botB
    sendToProcess b (cmd ++ "\n")
