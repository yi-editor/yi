{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Quit
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements quit commands.

module Yi.Keymap.Vim.Ex.Commands.Quit (parse) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable (find)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer
import           Yi.Core (quitEditor, errorEditor, closeWindow)
import           Yi.Editor
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types
import           Yi.Monad
import           Yi.String (showT)
import           Yi.Window (bufkey)


parse :: EventString -> Maybe ExCommand
parse = Common.parse $ P.choice
    [ do
        _ <- (P.try ( P.string "xit") <|> P.string "x")
        bangs <- P.many (P.char '!')
        return (quit True (not $ null bangs) False)
    , do
        ws <- P.many (P.char 'w')
        void $ P.try ( P.string "quit") <|> P.string "q"
        as <- P.many (P.try ( P.string "all") <|> P.string "a")
        bangs <- P.many (P.char '!')
        return $! quit (not $ null ws) (not $ null bangs) (not $ null as)
    ]

quit :: Bool -> Bool -> Bool -> ExCommand
quit w f a = Common.impureExCommand {
    cmdShow = (if w then "w" else "")
              `T.append` "quit"
              `T.append` (if a then "all" else "")
              `T.append` (if f then "!" else "")
  , cmdAction = YiA $ action w f a
  }

action :: Bool -> Bool -> Bool -> YiM ()
action False False False = quitWindowE
action False False  True = quitAllE
action  True False False = viWrite >> closeWindow
action  True False  True = saveAndQuitAllE
action False  True False = closeWindow
action False  True  True = quitEditor
action  True  True False = viWrite >> closeWindow
action  True  True  True = saveAndQuitAllE

quitWindowE :: YiM ()
quitWindowE = do
    nw <- withCurrentBuffer needsAWindowB
    ws <- withEditor $ use currentWindowA >>= windowsOnBufferE . bufkey
    if length ws == 1 && nw
       then errorEditor "No write since last change (add ! to override)"
       else closeWindow

quitAllE :: YiM ()
quitAllE = do
  a :| as <- readEditor bufferStack
  let needsWindow b = (b,) <$> withEditor (withGivenBuffer b needsAWindowB)
  bs <- mapM needsWindow (a:as)
  -- Vim only shows the first modified buffer in the error.
  case find snd bs of
      Nothing -> quitEditor
      Just (b, _) -> do
          bufferName <- withEditor $ withGivenBuffer b $ gets file
          errorEditor $ "No write since last change for buffer "
                        <> showT bufferName
                        <> " (add ! to override)"

saveAndQuitAllE :: YiM ()
saveAndQuitAllE = Common.forAllBuffers fwriteBufferE >> quitEditor

needsAWindowB :: BufferM Bool
needsAWindowB = do
  isWorthless <- gets (^. identA) >>= return . \case
    MemBuffer _ -> True
    FileBuffer _ -> False
  canClose <- gets isUnchangedBuffer
  return (not (isWorthless || canClose))
