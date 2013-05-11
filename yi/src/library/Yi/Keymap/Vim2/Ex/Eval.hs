module Yi.Keymap.Vim2.Ex.Eval
    ( exEvalE
    , exEvalY
    ) where

import Prelude ()
import Yi.Prelude

import Data.Either (either)
import Data.List (reverse)

import Yi.Buffer
import Yi.Core (closeWindow, quitEditor, errorEditor)
import Yi.Editor
import Yi.File (fwriteBufferE)
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Parse
import Yi.Search

executePureExCommand :: ExPureCommand -> EditorM ()
executePureExCommand (ExReplace from to flags) = withBuffer0 $ do
    let regex = makeSimpleSearch from
        replace = do
            region <- regionOfB Line
            discard $ searchAndRepRegion0 regex to (Global `elem` flags) region

    if EveryLine `elem` flags
    then withEveryLineB replace
    else replace

    moveToSol
executePureExCommand (ExGotoLine l) = discard $ withBuffer0 $ gotoLn l
executePureExCommand ExDelete =
    withBuffer0 $ do
        deleteUnitB Line Forward
        deleteN 1
executePureExCommand (ExGlobal term _ cmd) = do
    mark <- withBuffer0 setMarkHereB
    lineCount <- withBuffer0 lineCountB
    forM_ (reverse [1..lineCount]) $ \l -> do
        withBuffer0 $ gotoLn l
        executePureExCommand cmd
    withBuffer0 $ do
        getMarkPointB mark >>= moveTo
        deleteMarkB mark
executeImpureExCommand :: ExImpureCommand -> YiM ()
executeImpureExCommand (ExQuit False) = closeWindow
executeImpureExCommand (ExQuit True) = closeWindow
executeImpureExCommand (ExQuitAll True) = quitEditor
executeImpureExCommand (ExQuitAll False) = quitAllE
executeImpureExCommand (ExOpenFile f) = return ()

exEvalE :: String -> EditorM ()
exEvalE command =
    case (stringToExCommand command) of
        Left _ -> return ()
        Right (ExPure cmd) -> executePureExCommand cmd
        Right (ExImpure cmd) -> error $ "exEvalE got impure " ++ show cmd

exEvalY :: String -> YiM ()
exEvalY command =
    case (stringToExCommand command) of
        Left _ -> return ()
        Right (ExPure cmd) -> withEditor $ executePureExCommand cmd
        Right (ExImpure cmd) -> executeImpureExCommand cmd

quitAllE :: YiM ()
quitAllE = do
    bs <- mapM (\b -> (,) b <$> withEditor (withGivenBuffer0 b needsAWindowB)) =<< readEditor bufferStack
    -- Vim only shows the first modified buffer in the error.
    case find snd bs of
        Nothing -> quitEditor
        Just (b, _) -> do
            bufferName <- withEditor $ withGivenBuffer0 b $ gets file
            errorEditor $ "No write since last change for buffer "
                        ++ show bufferName
                        ++ " (add ! to override)"

saveAndQuitAllE :: YiM ()
saveAndQuitAllE = forAllBuffers fwriteBufferE >> quitEditor

needsAWindowB :: BufferM Bool
needsAWindowB = do
    isWorthless <- gets (either (const True) (const False) . (^. identA))
    canClose <- gets isUnchangedBuffer
    return (not (isWorthless || canClose))

forAllBuffers :: MonadEditor m => (BufferRef -> m ()) -> m ()
forAllBuffers f = mapM_ f =<< readEditor bufferStack

