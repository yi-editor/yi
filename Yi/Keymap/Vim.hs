--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- | Vim keymap for Yi. Emulates vim :set nocompatible
--

module Yi.Keymap.Vim ( keymap, VimMode ) where

import Yi.Yi hiding ( count )
import Yi.Style as Style
import Prelude       hiding ( any, error )

import Data.Char
import Data.List            ( (\\) )
import Data.Maybe           ( fromMaybe )

import Control.Exception    ( ioErrors, try, evaluate )
import Control.Monad.State

import Yi.Editor
import Yi.History
import Yi.Buffer
import Yi.Debug

import Yi.Indent


--
-- What's missing?
--   fancier :s//
--   '.'
--   movement parameterised \> \<
--

{-
  For now we just make the selected style the same as the 
  modeline_focused style... Just because i'm not good with
  styles yet - Jim
-}
defaultVimUiStyle :: Style.UIStyle
defaultVimUiStyle = Style.uiStyle { selected = Style.modeline_focused Style.uiStyle}

-- ---------------------------------------------------------------------

type VimMode = VimProc ()

type VimProc a = (Interact Char) a

                        
------------------------------------------------------------------------
--
-- | Top level. Lazily consume all the input, generating a list of
-- actions, which then need to be forced
--
-- NB . if there is a (bad) exception, we'll lose any new bindings.. iorefs?
--    . also, maybe we shouldn't refresh automatically?
--

--
-- | Top level. Lazily consume all the input, generating a list of
-- actions, which then need to be forced
--
-- NB . if there is a (bad) exception, we'll lose any new bindings.. iorefs?
--    . also, maybe we shouldn't refresh automatically?
--

keymap :: Keymap
keymap = do write $ setWindowFillE '~' >> withBuffer unsetMarkB >> setWindowStyleE defaultVimUiStyle
            runVim cmd_mode

runVim :: VimMode -> Keymap
runVim = comap eventToChar



------------------------------------------------------------------------

-- The vim lexer is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate lexers for each phase of key input in that mode.

-- | command mode consists of simple commands that take a count arg - the
-- count is stored in the lexer state. also the replace cmd, which
-- consumes one char of input, and commands that switch modes.
cmd_mode :: VimMode
cmd_mode = choice [cmd_eval,eval cmd_move,cmd2other,cmd_op]

eval :: YiAction m => VimProc (m ()) -> VimMode
eval p = do a <- p; write a

--
-- | insert mode is either insertion actions, or the meta \ESC action
--
ins_mode :: VimMode
ins_mode = write (msgE "-- INSERT --") >> many (ins_char +++ kwd_mode) >> event '\ESC' >> write msgClrE

--
-- | replace mode is like insert, except it performs writes, not inserts
--
rep_mode :: VimMode
rep_mode = write (msgE "-- REPLACE --") >> many rep_char >> event '\ESC' >> write msgClrE

--
-- | visual mode, similar to command mode
--
vis_mode :: VimMode
vis_mode = do 
  write (msgE "-- VISUAL --" >> withBuffer (pointB >>= setSelectionMarkPointB)) 
  many (eval cmd_move)
  (vis_multi +++ vis_single)
  write (msgClrE >> withBuffer unsetMarkB)


------------------------------------------------------------------------
--
-- A parser to accumulate digits.
-- typically what is needed for integer repetition arguments to commands
--
-- ToDo don't handle 0 properly
--
count :: VimProc (Maybe Int)
count = option Nothing (many1' (satisfy isDigit) >>= return . Just . read)

-- ---------------------------------------------------------------------
-- | Movement commands
--
-- The may be invoked directly, or sometimes as arguments to other
-- /operator/ commands (like d).
--

cmd_move :: VimProc (YiM ()) 
-- FIXME: This returns a general YiM action; however this makes sense only for buffer-only commands.
cmd_move = do 
  cnt <- count
  let x = maybe 1 id cnt
  choice ([event c >> return (a x) | (c,a) <- moveCmdFM] ++
          [do event c; c' <- anyButEsc; return (withBuffer (a x c')) | (c,a) <- move2CmdFM]) +++
   (do event 'G'; return $ withBuffer $ case cnt of 
                            Nothing -> botB >> moveToSol
                            Just n  -> gotoLn n >> return ())

--
-- TODO: Does this belong in CharMove.hs ?
--          Actually, more like Movement.hs
--
--   If we had each set of movement actions with
--   a common interface and a registered plugin, it
--   would make it really very configurable even by
--   nonprogrammers.. using a nice gui
--
detectMovement :: BufferM a -> BufferM Bool
detectMovement act = do x <- pointB
                        act
                        y <- pointB
                        if (x /= y) then return True
                                    else return False

-- The next 4 functions facilitate the default *word* concept 
-- of Vim. In vim, the iskeyword variable modifies this concept,
-- and it is configured differently for different filetypes.

-- TODO: word can be abstracted to textUnit to deal with 
--       all the various chunks of text that vim uses,
--       such as paragraph, WORD, sentence ...

-- sameWord c returns a function which tests other
--   characters to see if they can be part of the same
--   word
sameWord :: Char -> Char -> Bool
sameWord ch = head . filter ($ ch) $ [ type1, type2, type3 ]
              where type1 c = (isAlphaNum c) || (c == '_') -- vim has two types of words
                    type2 c = not (type1 c || type3 c)
                    type3 c = betweenWord c  -- actually I dont expect this should ever happen
                                             -- but i'll include it for robustness


betweenWord :: Char -> Bool
betweenWord ch = (isSpace ch) -- && (ch /= '\n')

begWord :: BufferM ()
begWord = do 
      moveWhileB (betweenWord) GoLeft
      c <- readB
      skippedAlpha <- detectMovement (moveWhileB (sameWord c) GoLeft)
      when skippedAlpha $ moveWhileB (not . sameWord c) GoRight

endWord :: BufferM ()
endWord = do 
      moveWhileB (betweenWord) GoRight
      c <- readB
      skippedAlpha <- detectMovement (moveWhileB (sameWord c) GoRight)
      when skippedAlpha $ moveWhileB (not.sameWord c) GoLeft

nextWord :: BufferM ()
nextWord = do 
      wasBetween <- detectMovement ( moveWhileB (betweenWord) GoRight)
      if wasBetween
         then return ()
         else do
            c <- readB
            moveWhileB (sameWord c) GoRight
            moveWhileB (betweenWord) GoRight

viewChar :: YiM ()
viewChar = do
   c <- withBuffer readB
   msgE . show $ c


--
-- movement commands
--
moveCmdFM :: [(Char, Int -> YiM ())]
moveCmdFM = 
-- left/right
    [('h',          left)
    ,('\^H',        left)
    ,(keyBackspace, left)
    ,('\BS',        left)
    ,('\127',       left)
    ,('l',          right)
    ,(' ',          right)
    ,(keyHome,      sol)
    ,('0',          sol)
    ,('^',          const $ withBuffer firstNonSpaceB)
    ,('$',          eol)
    ,(keyEnd,       eol)
    ,('|',          \i -> withBuffer (moveToSol >> moveXorEol (i-1)))

-- up/down
    ,('k',          up)
    ,(keyUp,        up)
    ,('\^P',        up)
    ,('j',          down)
    ,(keyDown,      down)
    ,('\^J',        down)
    ,('\^N',        down)
    ,('\r',         down)

-- words
    -- TODO: These handle blanks differently
    --       than vim .. (is our way better tho?)
    ,('w',          \i -> withBuffer $ replicateM_ i nextWord)
    ,('b',          \i -> withBuffer $ replicateM_ i (do
         moved <- detectMovement begWord
         when (not moved) (leftB >> begWord)
         ))
    ,('e',          \i -> withBuffer $ replicateM_ i (do
         moved <- detectMovement endWord
         when (not moved) (rightB >> endWord)
         ))



-- text
    ,('{',          withBuffer . prevNParagraphs)
    ,('}',          withBuffer . nextNParagraphs)

-- debugging
    ,('g',          \_ -> viewChar)

-- misc
    ,('H',          \i -> downFromTosE (i - 1))
    ,('M',          const middleE)
    ,('L',          \i -> upFromBosE (i - 1))

-- bogus entry
    ,('G',          const (return ()))
    ]
    where
        left  i = withBuffer $ moveXorSol i
        right i = withBuffer $ moveXorEol i
        up    i = withBuffer $ gotoLnFrom (-i) >> return ()
        down  i = withBuffer $ gotoLnFrom i    >> return ()
        sol   _ = withBuffer moveToSol
        eol   _ = withBuffer moveToEol

--
-- more movement commands. these ones are paramaterised by a character
-- to find in the buffer.
--
move2CmdFM :: [(Char, Int -> Char -> BufferM ())]
move2CmdFM =
    [('f',  \i c -> replicateM_ i $ nextCInc c)
    ,('F',  \i c -> replicateM_ i $ prevCInc c)
    ,('t',  \i c -> replicateM_ i $ nextCExc c)
    ,('T',  \i c -> replicateM_ i $ prevCExc c)
    ]

--
-- | Other command mode functions
--
cmd_eval :: VimMode
cmd_eval = do
   cnt <- count 
   let i = maybe 1 id cnt
   choice
    ([event c >> write (a i) | (c,a) <- singleCmdFM ] ++
    [events evs >> write (action i) | (evs, action) <- multiCmdFM ]) +++
    (do event 'r'; c <- anyButEscOrDel; write (writeB c)) +++
    (events ">>" >> write (shiftIndentOfLine i))+++
    (events "<<" >> write (shiftIndentOfLine (-i)))+++
    (events "ZZ" >> write (viWrite >> quitE))

anyButEscOrDel :: VimProc Char
anyButEscOrDel = oneOf $ any' \\ ('\ESC':delete')


--
-- cmd mode commands
--
singleCmdFM :: [(Char, Int -> YiM ())]
singleCmdFM =
    [('\^B',    upScreensE)             -- vim does (firstNonSpaceB;moveXorSol)
    ,('\^F',    downScreensE)
    ,('\^G',    const viFileInfo)        -- hmm. not working. duh. we clear
    ,('\^L',    const refreshE)
    ,('\^R',    withBuffer . flip replicateM_ redo)
    ,('\^Z',    const suspendE)
    ,('D',      const (withBuffer readRestOfLnB >>= setRegE >> withBuffer deleteToEol))
    ,('J',      const (withBuffer (moveToEol >> deleteB)))    -- the "\n"
    ,('U',      withBuffer . flip replicateM_ undo)    -- NB not correct
    ,('n',      const $ do getRegexE >>=
                               msgE . ("/" ++) . fst . fromMaybe ([],undefined)
                           searchE Nothing [] GoRight)
    ,('u',      withBuffer . flip replicateM_ undo)

    ,('X',      \i -> withBuffer $ do p <- pointB
                                      moveXorSol i
                                      q <- pointB
                                      when (p-q > 0) $ deleteN (p-q) )

    ,('x',      \i -> withBuffer $ do p <- pointB -- not handling eol properly
                                      moveXorEol i
                                      q <- pointB
                                      moveTo p
                                      when (q-p > 0) $ deleteN (q-p))

    ,('p',      (const $ do txt <- getRegE; withBuffer (moveXorEol 1 >> insertN txt >> leftB)))

    ,('P',      (const $ do txt <- getRegE; withBuffer (insertN txt >> leftB)))

    ,(keyPPage, upScreensE)
    ,(keyNPage, downScreensE)
    ,(keyLeft,  withBuffer . moveXorSol)
    ,(keyRight, withBuffer . moveXorEol)
    ,('~',      \i -> withBuffer $ do 
                         p <- pointB
                         moveXorEol i
                         q <- pointB
                         moveTo p
                         mapRangeB p q $ \c ->
                             if isUpper c then toLower c else toUpper c
                         moveTo q)
    ]

multiCmdFM :: [(String, Int -> YiM ())]
multiCmdFM =
    [("\^W\^C", const tryCloseE)
    ,("\^W\^O", const closeOtherE)
    ,("\^W\^S", const splitE)
    ,("\^W\^W", const nextWinE)
    ]
--
-- | So-called 'operators', which take movement actions as arguments.
--
-- How do we achive this? We look for the known operator chars
-- (op_char), then parse one of the known movement commands.
-- We then apply the returned action 
-- and then the operator. For example, we 'd'
-- command stores the current point, does a movement, then deletes from
-- the old to the new point.
cmd_op :: VimMode
cmd_op = do
  cnt <- count
  let i = maybe 1 id cnt
  choice $ [events "dd" >> write delCurLine,
            events "yy" >> write (withBuffer readLnB >>= setRegE)] ++
           [do event c; m <- cmd_move; write (a i m) | (c,a) <- opCmdFM]
    where
        -- | Used to implement the 'dd' command.
        delCurLine :: BufferM ()
        delCurLine = moveToSol >> deleteToEol >> deleteB >> 
                     atEof >>= flip when lineUp >> 
                     firstNonSpaceB

        -- | operator (i.e. movement-parameterised) actions
        opCmdFM :: [(Char,Int -> YiM () -> YiM ())]
        opCmdFM =
            [('d', \i m -> replicateM_ i $ do
                              (p,q) <- withPointMove m
                              withBuffer $ deleteN (max 0 (abs (q - p) + 1))  -- inclusive
             ),
             ('y', \_ m -> do (p,q) <- withPointMove m
                              s <- withBuffer (if p < q then readNM p q else readNM q p)
                              setRegE s -- ToDo registers not global.
             )]

        --
        -- A strange, useful action. Save the current point, move to
        -- some location specified by the sequence @m@, then return.
        -- Return the current, and remote point.
        --
        withPointMove :: YiM () -> YiM (Int,Int)
        withPointMove m = do p <- withBuffer pointB
                             m
                             q <- withBuffer pointB
                             when (p < q) $ withBuffer $ moveTo p
                             return (p,q)

--
-- | Switching to another mode from visual mode.
--
-- All visual commands are meta actions, as they transfer control to another
-- lexer. In this way vis_single is analogous to cmd2other
--
vis_single :: VimMode
vis_single =
        let beginIns a = do write (a >> withBuffer unsetMarkB) >> ins_mode
            yank = do txt <- withBuffer $ do
                         mrk <- getSelectionMarkPointB
                         pt <- pointB
                         readRegionB (mkVimRegion mrk pt)
                      setRegE txt
                      let rowsYanked = 1 + length (filter (== '\n') txt)
                      if rowsYanked > 2 then msgE ( (show rowsYanked) ++ " lines yanked")
                                        else return ()
            pasteOver = do text <- getRegE
                           withBuffer $ do 
                             mrk <- getSelectionMarkPointB
                             pt <- pointB
                             moveTo mrk
                             deleteRegionB (mkVimRegion mrk pt)
                             insertN text
            cut  = do txt <- withBuffer $ do
                        mrk <- getSelectionMarkPointB
                        pt <- pointB
                        txt <- readRegionB (mkVimRegion mrk pt)
                        deleteRegionB (mkVimRegion mrk pt)
                        return txt
                      setRegE txt
                      let rowsCut = 1 + length (filter (== '\n') txt)
                      if rowsCut > 2 then msgE ( (show rowsCut) ++ " fewer lines")
                                     else return ()
        in choice [
            event '\ESC' >> return (),
            event 'v'    >> return (),
            event ':'    >> ex_mode ":'<,'>",
            event 'y'    >> write yank,
            event 'x'    >> write cut,
            event 'p'    >> write pasteOver,
            event 'c'    >> beginIns cut]


--
-- | These also switch mode, as all visual commands do, but
-- | these are analogous to the commands in cmd_eval.
-- | They are different in that they are multiple characters
--
vis_multi :: VimMode
vis_multi = do
   cnt <- count 
   let i = maybe 1 id cnt
   choice ([events "ZZ" >> write (viWrite >> quitE),
            events ">>" >> write (shiftIndentOfSelection i),
            events "<<" >> write (shiftIndentOfSelection (-i)),
            do event 'r'; x <- anyEvent; write $ do
                                   mrk <- getSelectionMarkPointB
                                   pt <- pointB
                                   text <- readRegionB (mkVimRegion mrk pt)
                                   moveTo mrk
                                   deleteRegionB (mkVimRegion mrk pt)
                                   let convert '\n' = '\n'
                                       convert  _   = x
                                   insertN $ map convert $ text] ++
           [event c >> write (a i) | (c,a) <- singleCmdFM ])


--
-- | Switch to another vim mode from command mode.
--
-- These commands are meta actions, as they transfer control to another
-- lexer. Some of these commands also perform an action before switching.
--
cmd2other :: VimMode
cmd2other = let beginIns a = write a >> ins_mode
                beginIns :: YiM () -> VimMode
        in choice [
            do event ':'     ; ex_mode ":",
            do event 'v'     ; vis_mode,
            do event 'R'     ; rep_mode,
            do event 'i'     ; ins_mode,
            do event 'I'     ; beginIns (withBuffer moveToSol),
            do event 'a'     ; beginIns $ withBuffer $ moveXorEol 1,
            do event 'A'     ; beginIns (withBuffer moveToEol),
            do event 'o'     ; beginIns $ withBuffer $ moveToEol >> insertB '\n',
            do event 'O'     ; beginIns $ withBuffer $ moveToSol >> insertB '\n' >> lineUp,
            do event 'c'     ; beginIns $ not_implemented 'c',
            do event 'C'     ; beginIns $ withBuffer readRestOfLnB >>= setRegE >> withBuffer deleteToEol,
            do event 'S'     ; beginIns $ withBuffer (moveToSol >> readLnB) >>= setRegE >> withBuffer deleteToEol,
            do event '/'     ; ex_mode "/",
--          do event '?'   ; (with (not_implemented '?'), st{acc=[]}, Just $ cmd st),
            do event '\ESC'  ; write msgClrE,
            do event keyIC   ; ins_mode]


-- ---------------------------------------------------------------------
-- | vim insert mode
--
-- Some ideas for a better insert mode are contained in:
--
--      Poller and Garter , "A comparative study of moded and modeless
--      text editing by experienced editor users", 1983
--
-- which suggest that movement commands be added to insert mode, along
-- with delete.
--
ins_char :: VimMode
ins_char = write . fn =<< anyButEscOrCtlN
    where fn c = case c of
                    k | isDel k       -> withBuffer $ do s <- atSof
                                                         unless s (leftB >> deleteB)
                      | k == keyPPage -> upScreenE
                      | k == keyNPage -> downScreenE
                      | k == keyUp    -> withBuffer lineUp
                      | k == keyDown  -> withBuffer lineDown
                      | k == keyLeft  -> withBuffer leftB
                      | k == keyRight -> withBuffer rightB
                      | k == keyEnd   -> withBuffer moveToEol
                      | k == keyHome  -> withBuffer moveToSol
                    '\t' -> withBuffer $ insertTabB
                    _    -> withBuffer $ insertB c

anyButEscOrCtlN :: VimProc Char
anyButEscOrCtlN = oneOf $ (keyBackspace : any' ++ cursc') \\ ['\ESC','\^N']

-- --------------------
-- | Keyword 
--
kwd_mode :: VimMode
kwd_mode = many1' (event '\^N' >> write wordCompleteB) >> write resetCompleteB
-- Use many1' (not many1), otherwise resetCompleteE would always be chosen (because
-- it produces output earlier)


-- ---------------------------------------------------------------------
-- | vim replace mode
--
-- To quote vim:
--  In Replace mode, one character in the line is deleted for every character
--  you type.  If there is no character to delete (at the end of the line), the
--  typed character is appended (as in Insert mode).  Thus the number of
--  characters in a line stays the same until you get to the end of the line.
--  If a <NL> is typed, a line break is inserted and no character is deleted.
--
-- ToDo implement the undo features
--

rep_char :: VimMode
rep_char = write . fn =<< anyButEsc
    where fn c = case c of
                    k | isDel k       -> withBuffer leftB -- should undo unless pointer has been moved
                      | k == keyPPage -> upScreenE
                      | k == keyNPage -> downScreenE
                      | k == keyUp    -> (withBuffer lineUp)
                      | k == keyDown  -> (withBuffer lineDown)
                      | k == keyLeft  -> withBuffer leftB
                      | k == keyRight -> withBuffer rightB
                      | k == keyEnd   -> (withBuffer moveToEol)
                      | k == keyHome  -> (withBuffer moveToSol)
                    '\t' -> withBuffer $ insertN "    "
                    '\r' -> withBuffer $ insertB '\n'
                    _ -> withBuffer $ do e <- atEol
                                         if e then insertB c else writeB c >> rightB

-- ---------------------------------------------------------------------
-- Ex mode. We also process regex searching mode here.

spawn_ex_buffer :: String -> YiM ()
spawn_ex_buffer prompt = do
  -- The above ensures that the action is performed on the buffer that originated the minibuffer.
  let closeMinibuffer = do b <- withEditor getBuffer; closeE; withEditor $ deleteBuffer b 
      anyButDelNlArrow = oneOf $ any' \\ (enter' ++ delete' ++ ['\ESC',keyUp,keyDown])
      ex_buffer_finish = do 
        historyFinish
        lineString <- withBuffer elemsB
        closeMinibuffer
        ex_eval (head prompt : lineString)
      ex_process :: VimMode
      ex_process = 
          choice [do c <- anyButDelNlArrow; write $ (withBuffer . insertN) [c],
                  do enter; write ex_buffer_finish,
                  do event '\ESC'; write closeMinibuffer,
                  do delete; write bdeleteB,
                  do event keyUp; write historyUp,
                  do event keyDown; write historyDown]
  historyStart
  spawnMinibufferE prompt (const $ runVim $ ex_process) (return ())


ex_mode :: String -> VimMode
ex_mode = write . spawn_ex_buffer
                           
-- | eval an ex command to an YiM (), also appends to the ex history
ex_eval :: String -> YiM ()
ex_eval cmd = do
  case cmd of
        -- regex searching
          ('/':pat) -> searchE (Just pat) [] GoRight

        -- TODO: We give up on re-mapping till there exists a generic Yi mechanism to do so.

        -- add mapping to command mode
          (_:'m':'a':'p':' ':_cs) -> error "Not yet implemented."

        -- add mapping to insert mode
          (_:'m':'a':'p':'!':' ':_cs) -> error "Not yet implemented."

        -- unmap a binding from command mode
          (_:'u':'n':'m':'a':'p':' ':_cs) -> error "Not yet implemented."

        -- unmap a binding from insert mode
          (_:'u':'n':'m':'a':'p':'!':' ':_cs) -> error "Not yet implemented."


        -- just a normal ex command
          (_:src) -> fn src

        -- can't happen, but deal with it
          [] -> return ()

    where
      fn ""           = msgClrE

      fn s@(c:_) | isDigit c = do
        e <- lift $ try $ evaluate $ read s
        case e of Left _ -> errorE $ "The " ++show s++ " command is unknown."
                  Right lineNum -> withBuffer (gotoLn lineNum) >> return ()

      fn "w"          = viWrite
      fn ('w':' ':f)  = viWriteTo f
      fn "q"          = do b <- withBuffer isUnchangedB
                           if b then closeE
                                else errorE "No write since last change (add ! to override)"
      fn "q!"         = closeE
      fn "wq"         = viWrite >> closeE
      fn "x"          = do unchanged <- withBuffer isUnchangedB
                           unless unchanged viWrite
                           closeE
      fn "n"          = nextBufW
      fn "$"          = withBuffer botB
      fn "p"          = prevBufW
      fn ('s':'p':_)  = splitE
      fn ('e':' ':f)  = fnewE f
      fn ('s':'e':'t':' ':'f':'t':'=':ft)  = withBuffer $ setSyntaxB ft
      fn ('n':'e':'w':' ':f) = splitE >> fnewE f
      fn ('s':'/':cs) = viSub cs

      -- send just this line through external command /fn/
      fn ('.':'!':f) = do
            ln  <- withBuffer readLnB
            ln' <- pipeE f ln
            withBuffer $ do moveToSol
                            deleteToEol
                            insertN ln'
                            moveToSol

--    Needs to occur in another buffer
--    fn ('!':f) = pipeE f []

      fn "reboot"     = error "rebootE does not exist any more, use reloadE"     -- not in vim
      fn "reload"     = reloadE >> return ()    -- not in vim

      fn "redr"       = refreshE
      fn "redraw"     = refreshE

      fn "u"          = withBuffer undo
      fn "undo"       = withBuffer undo
      fn "r"          = withBuffer redo
      fn "redo"       = withBuffer redo

      fn "sus"        = suspendE
      fn "suspend"    = suspendE
      fn "st"         = suspendE
      fn "stop"       = suspendE

      fn s            = errorE $ "The "++show s++ " command is unknown."


------------------------------------------------------------------------

not_implemented :: Char -> YiM ()
not_implemented c = errorE $ "Not implemented: " ++ show c

-- ---------------------------------------------------------------------
-- Misc functions

viFileInfo :: YiM ()
viFileInfo = 
    do bufInfo <- withBuffer bufInfoB
       msgE $ showBufInfo bufInfo
    where 
    showBufInfo :: BufferFileInfo -> String
    showBufInfo bufInfo = concat [ show $ bufInfoFileName bufInfo
         , " Line "
         , show $ bufInfoLineNo bufInfo
         , " ["
         , bufInfoPercent bufInfo
         , "]"
         ]


-- | Try to write a file in the manner of vi\/vim
-- Need to catch any exception to avoid losing bindings
viWrite :: YiM ()
viWrite = do
    mf <- withBuffer getfileB
    case mf of
        Nothing -> errorE "no file name associate with buffer"
        Just f  -> do
            bufInfo <- withBuffer bufInfoB
            let s   = bufInfoFileName bufInfo
            let msg = msgE $ show f ++" "++show s ++ "C written"
            catchJustE ioErrors (fwriteToE f >> msg) (msgE . show)


-- | Try to write to a named file in the manner of vi\/vim
viWriteTo :: String -> YiM ()
viWriteTo f = do
    let f' = (takeWhile (/= ' ') . dropWhile (== ' ')) f
    bufInfo <- withBuffer bufInfoB
    let s   = bufInfoFileName bufInfo
    let msg = msgE $ show f'++" "++show s ++ "C written"
    catchJustE ioErrors (fwriteToE f' >> msg) (msgE . show)

-- | Try to do a substitution
viSub :: [Char] -> YiM ()
viSub cs = do
    let (pat,rep') = break (== '/')  cs
        (rep,opts) = case rep' of
                        []     -> ([],[])
                        (_:ds) -> case break (== '/') ds of
                                    (rep'', [])    -> (rep'', [])
                                    (rep'', (_:fs)) -> (rep'',fs)
    case opts of
        []    -> do_single pat rep
        ['g'] -> do_single pat rep
        _     -> do_single pat rep-- TODO

    where do_single p r = do
                s <- searchAndRepLocal p r
                if not s then errorE ("Pattern not found: "++p) else msgClrE

{-
          -- inefficient. we recompile the regex each time.
          -- stupido
          do_line   p r = do
                let loop i = do s <- searchAndRepLocal p r
                                if s then loop (i+1) else return i
                s <- loop (0 :: Int)
                if s == 0 then msgE ("Pattern not found: "++p) else msgClrE
-}

-- ---------------------------------------------------------------------
-- | Handle delete chars in a string
--
{-
clean :: [Char] -> [Char]
clean (_:c:cs) | isDel c = clean cs
clean (c:cs)   | isDel c = clean cs
clean (c:cs) = c : clean cs
clean [] = []
-}

-- | Is a delete sequence
isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False

-- ---------------------------------------------------------------------
-- | character ranges
--
delete, enter, anyButEsc :: VimProc Char
enter   = oneOf enter'
delete  = oneOf delete'
anyButEsc = oneOf $ (keyBackspace : any' ++ cursc') \\ ['\ESC']

enter', any', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']

cursc' :: [Char]
cursc' = [keyPPage, keyNPage, keyLeft, keyRight, keyDown, keyUp, keyHome, keyEnd]
