{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
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

import           Control.Applicative              (Alternative ((<|>)), (<$>))
import           Control.Lens                     (use, uses)
import           Control.Monad                    (void, when)
import           Data.Foldable                    (find)
import qualified Data.List.PointedList.Circular   as PL (length)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (append)
import qualified Text.ParserCombinators.Parsec    as P (char, choice, many, string, try)
import           Yi.Buffer                        (bkey, file)
import           Yi.Core                          (closeWindow, errorEditor, quitEditor)
import           Yi.Editor
import           Yi.File                          (deservesSave, fwriteAllY, viWrite)
import           Yi.Keymap                        (Action (YiA), YiM, readEditor)
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (impureExCommand, needsSaving, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Monad                         (gets)
import           Yi.String                        (showT)
import           Yi.Window                        (bufkey)


parse :: EventString -> Maybe ExCommand
parse = Common.parse $ P.choice
    [ do
        void $ P.try ( P.string "xit") <|> P.string "x"
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
    nw <- gets currentBuffer >>= Common.needsSaving
    ws <- withEditor $ use currentWindowA >>= windowsOnBufferE . bufkey
    if length ws == 1 && nw
       then errorEditor "No write since last change (add ! to override)"
       else do
         winCount <- withEditor $ uses windowsA PL.length
         tabCount <- withEditor $ uses tabsA PL.length
         if winCount == 1 && tabCount == 1
            -- if its the last window, quitting will quit the editor
            then quitAllE
            else closeWindow

quitAllE :: YiM ()
quitAllE = do
  let needsWindow b = (b,) <$> deservesSave b
  bs <- readEditor bufferSet >>= mapM needsWindow
  -- Vim only shows the first modified buffer in the error.
  case find snd bs of
      Nothing -> quitEditor
      Just (b, _) -> do
          bufferName <- withEditor $ withGivenBuffer (bkey b) $ gets file
          errorEditor $ "No write since last change for buffer "
                        <> showT bufferName
                        <> " (add ! to override)"

saveAndQuitAllE :: YiM ()
saveAndQuitAllE = do
    succeed <- fwriteAllY
    when succeed quitEditor
