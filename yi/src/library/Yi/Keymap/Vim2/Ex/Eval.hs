module Yi.Keymap.Vim2.Ex.Eval
    ( exEvalE
    , exEvalY
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types

exEvalE :: [String -> Maybe ExCommand] -> String -> EditorM ()
exEvalE cmds cmdString = evalHelper id (const $ error msg) cmds cmdString
    where msg = "exEvalE got impure command" ++ cmdString

exEvalY :: [String -> Maybe ExCommand] -> String -> YiM ()
exEvalY = evalHelper withEditor id

evalHelper :: MonadEditor m =>
    (EditorM () -> m ()) -> (YiM () -> m ()) ->
    [String -> Maybe ExCommand] -> String -> m ()
evalHelper pureHandler impureHandler cmds cmdString =
    case stringToExCommand cmds cmdString of
        Just cmd -> case cmdAction cmd of
                      Left pureAction -> pureHandler pureAction
                      Right impureAction -> impureHandler impureAction
        _ -> return ()
