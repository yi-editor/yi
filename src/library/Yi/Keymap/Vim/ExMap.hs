{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.ExMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- I'm a module waiting for some kind soul to give me a commentary!

module Yi.Keymap.Vim.ExMap (defExMap) where

import           Control.Applicative      ((<$), (<$>))
import           Control.Monad            (when)
import           Data.Char                (isSpace)
import           Data.Maybe               (fromJust)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T (Text, drop, head, length, split, unwords)
import           System.FilePath          (isPathSeparator)
import           Yi.Buffer.Adjusted       hiding (Insert)
import           Yi.Editor
import           Yi.History               (historyDown, historyFinish, historyPrefixSet, historyUp)
import           Yi.Keymap                (YiM)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Ex
import           Yi.Keymap.Vim.StateUtils (modifyStateE, resetCountE, switchModeE)
import           Yi.Keymap.Vim.Utils      (matchFromBool)
import qualified Yi.Rope                  as R (fromText, toText)
import           Yi.String                (commonTPrefix')

defExMap :: [EventString -> Maybe ExCommand] -> [VimBinding]
defExMap cmdParsers =
    [ exitBinding
    , completionBinding cmdParsers
    , finishBindingY cmdParsers
    , finishBindingE cmdParsers
    , failBindingE
    , historyBinding
    , printable
    ]

completionBinding :: [EventString -> Maybe ExCommand] -> VimBinding
completionBinding commandParsers = VimBindingY f
  where
    f "<Tab>" (VimState { vsMode = Ex }) = WholeMatch $ do
        commandString <- Ev . R.toText <$> withCurrentBuffer elemsB
        case evStringToExCommand commandParsers commandString of
          Just cmd -> complete cmd
          Nothing -> return ()
        return Drop
    f _ _ = NoMatch
    complete :: ExCommand -> YiM ()
    complete cmd = do
        possibilities <- cmdComplete cmd
        case possibilities of
          [] -> return ()
          (s:[]) -> updateCommand s
          ss -> do
              let s = commonTPrefix' ss
              updateCommand s
              printMsg . T.unwords . fmap (dropToLastWordOf s) $ ss

    updateCommand :: T.Text -> YiM ()
    updateCommand s = do
        withCurrentBuffer $ replaceBufferContent (R.fromText s)
        withEditor $ do
            historyPrefixSet s
            modifyStateE $ \state -> state {
                vsOngoingInsertEvents = Ev s
            }

-- | TODO: verify whether 'T.split' works fine here in place of
-- @split@'s 'splitWhen'. If something breaks then you should use
-- 'splitWhen' + 'T.pack'/'T.unpack'.
dropToLastWordOf :: T.Text -> T.Text -> T.Text
dropToLastWordOf s = case reverse . T.split isWordSep $ s of
  []     -> id
  [_]    -> id
  _ : ws -> T.drop . succ . T.length . T.unwords $ ws
  where
    isWordSep :: Char -> Bool
    isWordSep c = isPathSeparator c || isSpace c

exitEx :: Bool -> EditorM ()
exitEx success = do
    when success historyFinish
    resetCountE
    switchModeE Normal
    closeBufferAndWindowE
    withCurrentBuffer $ setVisibleSelection False

exitBinding :: VimBinding
exitBinding = VimBindingE f
    where
      f "<CR>" (VimState { vsMode = Ex, vsOngoingInsertEvents = Ev "" })
          = WholeMatch action
      f evs (VimState { vsMode = Ex })
          = action <$ matchFromBool (evs `elem` ["<Esc>", "<C-c>"])
      f _ _ = NoMatch
      action = exitEx False >> return Drop

finishBindingY :: [EventString -> Maybe ExCommand] -> VimBinding
finishBindingY commandParsers = VimBindingY f
    where f evs state = finishAction commandParsers exEvalY
                      <$ finishPrereq commandParsers (not . cmdIsPure) evs state


finishBindingE :: [EventString -> Maybe ExCommand] -> VimBinding
finishBindingE commandParsers = VimBindingE f
    where f evs state = finishAction commandParsers exEvalE
                      <$ finishPrereq commandParsers cmdIsPure evs state

finishPrereq :: [EventString -> Maybe ExCommand] -> (ExCommand -> Bool)
    -> EventString -> VimState -> MatchResult ()
finishPrereq commandParsers cmdPred evs s =
    matchFromBool . and $
        [ vsMode s == Ex
        , evs `elem` ["<CR>", "<C-m>"]
        , case evStringToExCommand commandParsers (vsOngoingInsertEvents s) of
            Just cmd -> cmdPred cmd
            _ -> False
        ]

finishAction :: MonadEditor m => [EventString -> Maybe ExCommand] ->
    ([EventString -> Maybe ExCommand] -> EventString -> m ()) -> m RepeatToken
finishAction commandParsers execute = do
  s <- withEditor $ withCurrentBuffer elemsB
  withEditor $ exitEx True
  execute commandParsers (Ev $ R.toText s) -- TODO
  return Drop

failBindingE :: VimBinding
failBindingE = VimBindingE f
    where f evs s | vsMode s == Ex && evs == "<CR>"
            = WholeMatch $ do
                exitEx False
                state <- getEditorDyn
                printMsg . _unEv $ "Not an editor command: " <> vsOngoingInsertEvents state
                return Drop
          f _ _ = NoMatch

printable :: VimBinding
printable = VimBindingE f
    where f evs (VimState { vsMode = Ex }) = WholeMatch $ editAction evs
          f _ _ = NoMatch

historyBinding :: VimBinding
historyBinding = VimBindingE f
    where f evs (VimState { vsMode = Ex }) | evs `elem` fmap fst binds
             = WholeMatch $ do
              fromJust $ lookup evs binds
              command <- withCurrentBuffer elemsB
              modifyStateE $ \state -> state {
                  vsOngoingInsertEvents = Ev $ R.toText command
              }
              return Drop
          f _ _ = NoMatch
          binds =
              [ ("<Up>", historyUp)
              , ("<C-p>", historyUp)
              , ("<Down>", historyDown)
              , ("<C-n>", historyDown)
              ]

editAction :: EventString -> EditorM RepeatToken
editAction (Ev evs) = do
  withCurrentBuffer $ case evs of
      "<BS>"  -> bdeleteB
      "<C-h>" -> bdeleteB
      "<C-w>" -> do
          r <- regionOfPartNonEmptyB unitViWordOnLine Backward
          deleteRegionB r
      "<C-r>" -> return () -- TODO
      "<lt>" -> insertB '<'
      "<Del>" -> deleteB Character Forward
      "<Left>" -> moveXorSol 1
      "<C-b>" -> moveXorSol 1
      "<Right>" -> moveXorEol 1
      "<C-f>" -> moveXorEol 1
      "<Home>" -> moveToSol
      "<C-a>" -> moveToSol
      "<End>" -> moveToEol
      "<C-e>" -> moveToEol
      "<C-u>" -> moveToSol >> deleteToEol
      "<C-k>" -> deleteToEol
      evs' -> case T.length evs' of
        1 -> insertB $ T.head evs'
        _ -> error $ "Unhandled event " ++ show evs' ++ " in ex mode"
  command <- R.toText <$> withCurrentBuffer elemsB
  historyPrefixSet command
  modifyStateE $ \state -> state {
    vsOngoingInsertEvents = Ev command
  }
  return Drop
