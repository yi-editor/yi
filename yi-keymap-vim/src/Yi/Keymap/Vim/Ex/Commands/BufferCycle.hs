{-# LANGUAGE OverloadedStrings #-}
module Yi.Keymap.Vim.Ex.Commands.BufferCycle (parse) where

import           Control.Applicative                 (Alternative ((<|>)))
import           Control.Monad
import           Data.List
import qualified Data.Attoparsec.Text             as P
import           Lens.Micro.Platform
import           Yi.Editor
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Window
import           Yi.Keymap.Vim.Common                (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
  void (P.char 'b') <|> return ()
  direction <- (fmap (const False) $ P.string "prev" <|> P.string "previous") <|> fmap (const True) (P.string "next")
  return $ Common.pureExCommand {
    cmdShow = if direction then "next" else "previous",
    cmdAction = EditorA $ currentWindowA %= \window -> let
      cl = bufkey window : bufAccessList window
      cl' = if direction
        then last cl : init cl
        else tail cl ++ [head cl]
      in forceSpine cl' `seq` window { bufkey = head cl', bufAccessList = tail cl' }
   }

{-# INLINE forceSpine #-}
forceSpine :: [a] -> ()
forceSpine = go where
  go [] = ()
  go (_:r) = go r
