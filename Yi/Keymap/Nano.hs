-- Copyright (c) 2004, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | An emulation of the Nano editor

module Yi.Keymap.Nano ( keymap ) where

import Yi.Yi
import Yi.Keymap.Emacs.KillRing

import Data.Char            ( chr, isAlphaNum, toLower )
import Control.Arrow

import Control.Exception    ( ioErrors, try, evaluate )
import Control.Monad ( when )
import Control.Monad.Trans ( lift )

-- | Top level function. A function of this type is used by the editor
-- main loop to interpret actions. The second argument to @execLexer@ is
-- our default state.

keymap :: Keymap
keymap = comap eventToChar nano_km

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
             write $ insertN c

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
    [
     -- ('\127',       write $ bdeleteB1), -- ?
    ('\188',       write prevBufW)    -- 'M-<' ?
    ,('\190',       write nextBufW)    -- 'M->' ?
    ,('\^A',        write moveToSol)
    ,('\^B',        write leftB)
    ,('\^D',        write deleteN 1)
    ,('\^E',        write moveToEol)
    ,('\^F',        write rightB)
    ,('\^H',        write $ leftB >> deleteN 1)
    ,('\^K',        write $ killLineE)
    ,('\^L',        write refreshE)
    ,('\^M',        write $ insertN '\n')
    ,('\^N',        write (execB Move VLine Forward))
    ,('\^P',        write (execB Move VLine Backward))
    ,('\^U',        write undoB)
    ,('\^V',        write downScreenB)
    ,('\^X',        do write $ do b <- isUnchangedB ; if b then quitE else return ()
                       switch2WriteMode) -- TODO: separate this
    ,('\^Y',        write upScreenB)
    ,('\^Z',        write suspendE)
    ,('\0',         write $ do moveWhileE (isAlphaNum)      GoRight
                               moveWhileE (not . isAlphaNum)  GoRight )
    ,(keyBackspace, write $ leftB >> deleteN 1)
    ,(keyDown,      write (execB Move VLine Backward))
    ,(keyLeft,      write leftB)
    ,(keyRight,     write rightB)
    ,(keyUp,        write (execB Move VLine Backward))
    ,('\^G',        write $ msgE "nano-yi : yi emulating nano")
    ,('\^I',        write (do bufInfo <- bufInfoB
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
            write $ msgE "Save modified buffer (ANSWERING \"No\" WILL DESTROY CHANGES) ? "
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
  write $ do mre <- getRegexE
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

    srch_y = event '\^Y' >> write (gotoLn 0 >> moveToSol)
    srch_v = event '\^V' >> write (do bufInfo <- bufInfoB
                                      let x = bufInfoLineNo bufInfo
                                      gotoLn x >> moveToSol)

    srch_t = event '\^T' >> write (msgE "unimplemented") -- goto line

    srch_c = event '\^C' >> write (msgE "[ Search Cancelled ]")
    srch_r = event '\^R' >> write (msgE "unimplemented")

    performSearch = event '\n' >> write (case p of
                                           [] -> doSearch Nothing  [] GoRight
                                           _  -> doSearch (Just p) [] GoRight)

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
  result <- lineEdit []
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
            then return ()
            else catchJustE ioErrors (do fwriteToE f ; msgE "Wrote current file.")
                                     (msgE . show)
     ,"File Name to Write: ")

    ,('\^_',
     \s -> do e <- lift $ try $ evaluate $ read s
              case e of Left _   -> errorE "[ Come on, be reasonable ]"
                        Right ln -> gotoLn ln >> moveToSol >> msgClrE
     ,"Enter line number: ")
    ]

-- ---------------------------------------------------------------------
-- utilities

-- undef :: Char -> Action
-- undef c = errorE $ "Not implemented: " ++ show c

deleteChars :: [Char]
deleteChars  = ['\BS', '\127', keyBackspace]

