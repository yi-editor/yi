{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Eval
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Eval
    ( exEvalE
    , exEvalY
    ) where

import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import           Yi.Editor
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Ex.Types

exEvalE :: [EventString -> Maybe ExCommand] -> EventString -> EditorM ()
exEvalE cmds cmdString = evalHelper id (const $ error msg) cmds cmdString
    where msg = T.unpack . _unEv $ "exEvalE got impure command" <> cmdString

exEvalY :: [EventString -> Maybe ExCommand] -> EventString -> YiM ()
exEvalY = evalHelper withEditor id

evalHelper :: MonadEditor m =>
    (EditorM () -> m ()) -> (YiM () -> m ()) ->
    [EventString -> Maybe ExCommand] -> EventString -> m ()
evalHelper pureHandler impureHandler cmds cmdString =
    case evStringToExCommand cmds cmdString of
        Just cmd -> case cmdAction cmd of
            BufferA actionB -> pureHandler $ withCurrentBuffer (void actionB)
            EditorA actionE -> pureHandler (void actionE)
            YiA actionY -> impureHandler (void actionY)
        _ -> return ()
