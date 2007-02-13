--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- | An emulation of the Nano editor
--

module Yi.Keymap.Nano ( keymap ) where

import Yi.Yi hiding         ( keymap )

import Data.Char            ( chr, isAlphaNum, toLower )

import Control.Exception    ( ioErrors, catchJust, try, evaluate )
import Control.Monad ( when )

--
-- | Top level function. A function of this type is used by the editor
-- main loop to interpret actions. The second argument to @execLexer@ is
-- our default state.
--
keymap :: Keymap
keymap = runProcess nano_km . map eventToChar

-- | @NanoMode@  is the type of our interactive process.
type NanoMode = Interact Char ()

-- | Helper function: match any char in the input string.
anyChar :: String -> Interact Char Char
anyChar cs = satisfy (`elem` cs)

-- | The default mode is /cmd/ mode. Our other mode is the echo buffer
-- mode. In cmd mode you can insert chars, run commands and switch to
-- the echo buffer mode.
--
nano_km :: NanoMode
nano_km = choice [cmdChar, cmdSwitch, searchChar, insChar]

--
-- Here's where we write a bunch of lexer fragments, corresponding to
-- the different behaviours of the editor: char insertion, cmd actions,
-- cmd line buffer editing, etc.
--

--
-- | Normal chars just insert themselves
--
insChar :: NanoMode
insChar = do c <- anyChar ('\n' : map chr [32 .. 126])
             write $ insertE c

--
-- | Command chars run actions.
--
cmdChar :: NanoMode
cmdChar = choice [event c >> act | (c,act) <- cmdCharFM]

--
-- A key\/action table. This is where we actually map command (^) chars
-- to actions.
--
cmdCharFM :: [(Char, NanoMode)]
cmdCharFM = 
    [('\127',       write $ leftE >> deleteE)
    ,('\188',       write prevBufW)    -- 'M-<' ?
    ,('\190',       write nextBufW)    -- 'M->' ?
    ,('\^A',        write solE)
    ,('\^B',        write leftE)
    ,('\^D',        write deleteE)
    ,('\^E',        write eolE)
    ,('\^F',        write rightE)
    ,('\^H',        write $ leftE >> deleteE)
    ,('\^J',        write $ undef '\^J')
    ,('\^K',        write $ readRestOfLnE >>= setRegE >> killE)
    ,('\^L',        write refreshE)
    ,('\^M',        write $ insertE '\n')
    ,('\^N',        write downE)
    ,('\^P',        write upE)
    ,('\^U',        write undoE)
    ,('\^V',        write downScreenE)
    ,('\^X',        do write $ do b <- isUnchangedE ; if b then quitE else return ()
                       switch2WriteMode) -- TODO: separate this 
    ,('\^Y',        write upScreenE)
    ,('\^Z',        write suspendE)
    ,('\0',         write $ do moveWhileE (isAlphaNum)      GoRight
                               moveWhileE (not . isAlphaNum)  GoRight )
    ,(keyBackspace, write $ leftE >> deleteE)
    ,(keyDown,      write downE)
    ,(keyLeft,      write leftE)
    ,(keyRight,     write rightE)
    ,(keyUp,        write upE)
    ,('\^G',        write $ msgE "nano-yi : yi emulating nano")
    ,('\^I',        write (do bufInfo <- bufInfoE
		              let s   = bufInfoFileName bufInfo
		                  ln  = bufInfoLineNo   bufInfo
                                  col = bufInfoColNo    bufInfo
		                  pt  = bufInfoCharNo   bufInfo
		                  pct = bufInfoPercent  bufInfo
                              msgE $ "[ line "++show ln++", col "++show col++
		                     ", char "++show pt++"/"++show s++" ("++pct++") ]"))
    ]

    where
        --
        -- | print a message and switch to sub-mode lexer for Y\/N questions
        --
        switch2WriteMode = do
            write $ do msgE "Save modified buffer (ANSWERING \"No\" WILL DESTROY CHANGES) ? "
                       cmdlineFocusE
            c <- anyChar "ynYN"
            when (toLower c == 'y') $ write fwriteE
            write quitE

--
-- | Switching to the command buffer
--
-- help-mode in nano to be a separate mode too. There are probably
-- others.
--
-- We see a \^R, we need to print a prompt, and switch to
-- the new mode, which will accumulate filename characters until nl.
--  Once the user presses Enter, in the @echo_km@ mode, we are
-- able to evaluate the action char represents (e.g. writing a file).
--
-- Since we're going to edit the command buffer, which is still a
-- slighly buffer magic (unfortunately) we need to explicitly switch
-- focus.
--
cmdSwitch :: NanoMode
cmdSwitch = choice [event c >> echoMode prompt (\s -> anyChar "\n\r" >> write (act s))
                    | (c,act,prompt) <- echoCharFM]

--
-- | Nano search behaviour.
--

searchChar :: NanoMode
searchChar = do 
  event '\^W'
  write $ do cmdlineFocusE
             mre <- getRegexE
             let prompt = case mre of     -- create a prompt
                    Nothing      -> "Search: "
                    Just (pat,_) -> "Search ["++pat++"]: "
             msgE prompt  
             -- FIXME: the prompt currently cannot be passed to the echoMode, this prompt will get overwritten.
             -- The fix is NOT to use MetaM!!!
             -- The fix is to stop using getRegexE to remember the last thing searched.
  echoMode "Search: " search_km 
  return ()

--
-- When searching, a few extra key bindings become available, which
-- immediately interrupt the echo mode, perform an action, and then drop
-- back to normal mode.
--
-- ^G Get Help ^Y First Line  ^R Replace     M-C Case Sens  M-R Regexp
-- ^C Cancel   ^V Last Line   ^T Go To Line  M-B Direction  Up History
--
-- We augment the echo keymap with the following bindings, by passing
-- them in the @OnlyMode@ field of the lexer state. The echo keymap the
-- knows how to add in these extra bindings.
--
search_km :: String -> NanoMode
search_km p = choice [srch_g, srch_y, srch_v, srch_t, srch_c, srch_r, performSearch]
  where -- TODO: use the same style as other modes (list of Char, String -> Action)
    srch_g = event '\^G' >> write (msgE "nano-yi : yi emulating nano")

    srch_y = event '\^Y' >> write (gotoLnE 0 >> solE)
    srch_v = event '\^V' >> write (do bufInfo <- bufInfoE
				      let x = bufInfoLineNo bufInfo
                                      gotoLnE x >> solE)

    srch_t = event '\^T' >> write (msgE "unimplemented") -- goto line

    srch_c = event '\^C' >> write (msgE "[ Search Cancelled ]")
    srch_r = event '\^R' >> write (msgE "unimplemented")

    performSearch = event '\n' >> write (case p of
                                           [] -> searchE Nothing  [] GoRight
                                           _  -> searchE (Just p) [] GoRight)

    -- M-C
    -- M-R
    -- M-B
    -- Up

------------------------------------------------------------------------
--
-- echo buffer mode
--

-- | A simple line editor. 
-- @echoMode prompt exitProcess@ runs the line editor; @prompt@ will
-- be displayed as prompt, @exitProcess@ is a process that will be
-- used to exit the line-editor sub-process if it succeeds on input
-- typed during edition.

echoMode :: String -> (String -> Interact Char a) -> Interact Char a
echoMode prompt exitProcess = do 
  write (logPutStrLn "echoMode")
  write cmdlineFocusE 
  result <- lineEdit []
  write cmdlineUnFocusE
  return result
    where lineEdit s =
              do write $ msgE (prompt ++ s)
                 (exitProcess s +++
                  (anyChar deleteChars >> lineEdit (take (length s - 1) s)) +++
                  (do c <- anyChar ('\n' : map chr [32 .. 126]); lineEdit (s++[c])))
                   
-- | Actions that mess with the echo (or command) buffer. Notice how
-- these actions take a @String@ as an argument, and the second
-- component of the elem of the fm is a string that is used as a
-- prompt.

echoCharFM :: [(Char, String -> Action, String)]
echoCharFM =
    [('\^O',
      \f -> if f == []
            then nopE
            else catchJust ioErrors (do fwriteToE f ; msgE "Wrote current file.")
                                    (msgE . show)
     ,"File Name to Write: ")

    ,('\^_',
     \s -> do e <- try $ evaluate $ read s
              case e of Left _   -> errorE "[ Come on, be reasonable ]"
                        Right ln -> gotoLnE ln >> solE >> msgClrE
     ,"Enter line number: ")
    ]

-- ---------------------------------------------------------------------
-- utilities

undef :: Char -> Action
undef c = errorE $ "Not implemented: " ++ show c

deleteChars :: [Char]
deleteChars  = ['\BS', '\127', keyBackspace]

