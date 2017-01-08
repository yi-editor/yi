{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.BufferDelete
-- License     :  GPL-2
--
-- :reg[isters] ex command to list yanked texts.
module Yi.Keymap.Vim.Ex.Commands.Registers (printRegisters, parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad                    (void)
import           Data.Monoid                      ((<>))
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Keymap.Vim.Common             (EventString, RegisterName, Register (regContent), VimState (vsRegisterMap))
import           Yi.Editor                        (EditorM, getEditorDyn, newBufferE)
import           Yi.Rope                          (YiString)
import           Yi.Types                         (withEditor, BufferId (MemBuffer))
import qualified Data.Attoparsec.Text             as P (string, try, endOfInput)
import qualified Data.HashMap.Strict              as HM (toList)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import qualified Yi.Rope                          as R (concat, toString, fromString)


-- | Show registered register and content in new buffer
printRegisters :: EditorM ()
printRegisters = do
  xs <- HM.toList . vsRegisterMap <$> getEditorDyn
  let xs'       = visualizeConvert xs
      registers = flip map xs' $ \(nameWithSep, content) -> nameWithSep <> content <> "\n"
      bufDetail = "--- Register ---\n" <> R.concat registers
  void $ newBufferE (MemBuffer "Register list") bufDetail
  where
    replaceName n | n == '\NUL' = "\\NUL | "
                  | otherwise   = ['"', n] ++ "   | "  -- Straighten diff of \NUL
    replaceContent = let replaceContentChar c | c == '\n' = "^J"
                                              | otherwise = [c]
                     in concatMap replaceContentChar
    visualizeConvert :: [(RegisterName, Register)] -> [(YiString, YiString)]
    visualizeConvert = map $ \(name, reg) ->
      let content = R.toString . regContent $ reg
      in ( R.fromString . replaceName $ name
         , R.fromString . replaceContent $ content
         )


-- | See :help :registers on Vim
parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
  _ <- P.string "reg" <* (     P.try (P.string "isters")
                      <|> P.try (P.string "ister")
                      <|> P.try (P.string "iste")
                      <|> P.try (P.string "ist")
                      <|> P.try (P.string "is")
                      <|> P.try (P.string "i")
                      <|> P.string ""
                    )
                 <* P.endOfInput
  return Common.pureExCommand
    { cmdShow   = "registers"
    , cmdAction = EditorA $ withEditor printRegisters
    }
