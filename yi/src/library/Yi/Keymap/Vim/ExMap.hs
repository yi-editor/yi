module Yi.Keymap.Vim.ExMap
    ( defExMap
    ) where

import Control.Applicative
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.List.Split (splitWhen)
import System.FilePath (isPathSeparator)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.History
import Yi.Keymap
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.Utils
import Yi.Keymap.Vim.Ex
import Yi.Utils

defExMap :: [String -> Maybe ExCommand] -> [VimBinding]
defExMap cmdParsers =
    [ exitBinding
    , completionBinding cmdParsers
    , finishBindingY cmdParsers
    , finishBindingE cmdParsers
    , failBindingE
    , historyBinding
    , printable
    ]

completionBinding :: [String -> Maybe ExCommand] -> VimBinding
completionBinding commandParsers = VimBindingY f
    where f "<Tab>" (VimState { vsMode = Ex }) = WholeMatch $ do
              commandString <- withEditor . withBuffer0 $ elemsB
              case stringToExCommand commandParsers commandString of
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
                    let s = commonPrefix ss
                    updateCommand s
                    withEditor
                        . printMsg
                        . unwords
                        . fmap (dropToLastWordOf s)
                        $ ss
          updateCommand :: String -> YiM ()
          updateCommand s = do
              withBuffer $ replaceBufferContent s
              withEditor $ do
                  historyPrefixSet s
                  modifyStateE $ \state -> state {
                      vsOngoingInsertEvents = s
                  }

dropToLastWordOf :: String -> String -> String
dropToLastWordOf s =
    case reverse . splitWhen isWordSep $ s of
        [] -> id
        (_w:[]) -> id
        (_w:ws) -> drop . (+1) . length . unwords $ ws
    where
        isWordSep :: Char -> Bool
        isWordSep c = isPathSeparator c || isSpace c

exitEx :: Bool -> EditorM ()
exitEx success = do
    when success historyFinish
    resetCountE
    switchModeE Normal
    closeBufferAndWindowE

exitBinding :: VimBinding
exitBinding = VimBindingE f
    where f "<CR>" (VimState { vsMode = Ex, vsOngoingInsertEvents = [] })
              = WholeMatch action
          f evs (VimState { vsMode = Ex })
              = action <$ matchFromBool (evs `elem` ["<Esc>", "<C-c>"])
          f _ _ = NoMatch
          action = exitEx False >> return Drop

finishBindingY :: [String -> Maybe ExCommand] -> VimBinding
finishBindingY commandParsers = VimBindingY f
    where f evs state = finishAction commandParsers exEvalY
                      <$ finishPrereq commandParsers (not . cmdIsPure) evs state
    

finishBindingE :: [String -> Maybe ExCommand] -> VimBinding
finishBindingE commandParsers = VimBindingE f
    where f evs state = finishAction commandParsers exEvalE
                      <$ finishPrereq commandParsers cmdIsPure evs state

finishPrereq :: [String -> Maybe ExCommand] -> (ExCommand -> Bool)
    -> EventString -> VimState -> MatchResult ()
finishPrereq commandParsers cmdPred evs s =
    matchFromBool . and $
        [ vsMode s == Ex
        , evs == "<CR>"
        , case stringToExCommand commandParsers (vsOngoingInsertEvents s) of
            Just cmd -> cmdPred cmd
            _ -> False
        ]

finishAction :: MonadEditor m => [String -> Maybe ExCommand] ->
    ([String -> Maybe ExCommand] -> String -> m ()) -> m RepeatToken
finishAction commandParsers execute = do
    s <- withEditor $ withBuffer0 elemsB
    withEditor $ exitEx True
    execute commandParsers s
    return Drop

failBindingE :: VimBinding
failBindingE = VimBindingE f
    where f evs s | vsMode s == Ex && evs == "<CR>"
            = WholeMatch $ do
                  exitEx False
                  state <- getDynamic
                  printMsg $ "Not an editor command: " ++ vsOngoingInsertEvents state
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
              command <- withBuffer0 elemsB
              modifyStateE $ \state -> state {
                  vsOngoingInsertEvents = command
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
editAction evs = do
    withBuffer0 $ case evs of
        (c:[]) -> insertB c
        "<BS>"  -> deleteB Character Backward
        "<C-h>" -> deleteB Character Backward
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
        evs' -> error $ "Unhandled event " ++ evs' ++ " in ex mode"
    command <- withBuffer0 elemsB
    historyPrefixSet command
    modifyStateE $ \state -> state {
        vsOngoingInsertEvents = command
    }
    return Drop
