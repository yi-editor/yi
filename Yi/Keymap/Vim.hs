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

import Yi.Region
import Yi.Core
import Yi.CharMove
import Yi.Interact   hiding ( count )
import Yi.Debug
import Yi.UI
import Yi.Style as Style
import Yi.Search
import Yi.Keymap

import Prelude       hiding ( any, error )

import Data.Char
import Data.List            ( (\\) )
import Data.Maybe           ( fromMaybe )

import Control.Exception    ( ioErrors, try, evaluate )
import Control.Monad.State

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

type VimProc a = StateT VimState (Interact Char) a


--
-- state threaded through the lexer
--
-- In vi, you may add bindings (:map) to cmd or insert mode. We thus
-- carry around the current cmd and insert lexers in the state. Calls to
-- switch editor modes therefore use the lexers in the state.
--
data VimState =
        St { hist :: ([String],Int) -- ex-mode command history
           , cmdMode :: VimMode      -- (maybe augmented) cmd mode lexer
           , ins :: VimMode }        -- (maybe augmented) ins mode lexer

------------------------------------------------------------------------
--
-- | Settings variables. These should really be stored in a mutable map.
--
shiftwidth :: Int
shiftwidth = 4 -- sw
expandTabs :: Bool
expandTabs = True -- et
tabsize :: Int
tabsize = 8  -- ts

-- | tabifySpacesOnLineAndShift num 
-- |  shifts right (or left if num is negative) num times, filling in tabs if
-- |  expandtabs is set.
tabifySpacesOnLineAndShift :: Int -> Action
tabifySpacesOnLineAndShift numOfShifts = 
                     do solE
                        sol <- getPointE
                        firstNonSpaceE
                        -- ptOfNonSpace <- getPointE
                        atSol <- atSolE 
                        if (not atSol) then leftE
                                       else nopE
                        ptOfLastSpace <- getPointE
                        msgE ("ptOfLastSpace= " ++ (show ptOfLastSpace) ++ "-" ++ (show sol) ++ "=" ++ (show (ptOfLastSpace - sol)))
                        let countSpace '\t' = tabsize
                            countSpace _ = 1 -- we'll assume nothing but tabs and spaces
                        cnt <- if (atSol) then return 0
                                            else readRegionE (mkVimRegion sol ptOfLastSpace) >>= return . sum . map countSpace
                        if (not atSol) then deleteRegionE (mkVimRegion sol ptOfLastSpace)
                                       else nopE

                        let newcount = cnt + (shiftwidth * numOfShifts)
                        if (newcount <= 0)
                           then nopE
                           else do
                             let tabs   = replicate (newcount `div` tabsize) '\t'
                                 spaces = replicate (newcount `mod` tabsize) ' '
                             solE
                             insertNE $ if expandTabs then replicate newcount ' '
                                                      else tabs ++ spaces
                                                                                  
                             firstNonSpaceE
                        
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
keymap cs = setWindowFillE '~' : winStyleAct : runProcess (runStateT cmd_mode defaultSt) (map eventToChar cs)
    where
      winStyleAct = unsetMarkE >> setWindowStyleE defaultVimUiStyle

-- | default lexer state, just the normal cmd and insert mode. no mappings
defaultSt :: VimState
defaultSt = St { hist = ([],0), cmdMode = cmd_mode, ins = ins_mode }

------------------------------------------------------------------------

-- The vim lexer is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate lexers for each phase of key input in that mode.

-- | command mode consits of simple commands that take a count arg - the
-- count is stored in the lexer state. also the replace cmd, which
-- consumes one char of input, and commands that switch modes.
cmd_mode :: VimMode
cmd_mode = forever (choice [cmd_eval,eval cmd_move,cmd2other,cmd_op])

eval :: VimProc Action -> VimMode
eval p = do a <- p; write a

--
-- | insert mode is either insertion actions, or the meta \ESC action
--
ins_mode :: VimMode
ins_mode = write (msgE "-- INSERT --") >> many' (ins_char +++ kwd_mode) >> event '\ESC' >> write msgClrE

--
-- | replace mode is like insert, except it performs writes, not inserts
--
rep_mode :: VimMode
rep_mode = write (msgE "-- REPLACE --") >> many' rep_char >> event '\ESC' >> write msgClrE

--
-- | visual mode, similar to command mode
--
vis_mode :: VimMode
vis_mode = do 
  modify (\st->st{cmdMode=vis_mode})
  write (msgE "-- VISUAL --" >> getPointE >>= setMarkE) 
  many' (eval cmd_move)
  (vis_multi +++ vis_single)
  write (msgClrE >> unsetMarkE)


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

cmd_move :: VimProc Action
cmd_move = do 
  cnt <- count
  let x = maybe 1 id cnt
  choice ([event c >> return (a x) | (c,a) <- moveCmdFM] ++
          [do event c; c' <- anyButEsc; return (a x c') | (c,a) <- move2CmdFM]) +++
   (do event 'G'; return $ case cnt of 
                            Nothing -> botE >> solE
                            Just n  -> gotoLnE n)

--
-- TODO: Does this belong in CharMove.hs ?
--          Actually, more like Movement.hs
--
--   If we had each set of movement actions with
--   a common interface and a registered plugin, it
--   would make it really very configurable even by
--   nonprogrammers.. using a nice gui
--
detectMovement :: Action -> EditorM Bool
detectMovement act = do x <- getPointE
                        act
                        y <- getPointE
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

begWord :: Action
begWord = do 
      moveWhileE (betweenWord) GoLeft
      c <- readE
      skippedAlpha <- detectMovement (moveWhileE (sameWord c) GoLeft)
      when skippedAlpha $ moveWhileE (not.sameWord c) GoRight

endWord :: Action
endWord = do 
      moveWhileE (betweenWord) GoRight
      c <- readE
      skippedAlpha <- detectMovement (moveWhileE (sameWord c) GoRight)
      when skippedAlpha $ moveWhileE (not.sameWord c) GoLeft

nextWord :: Action
nextWord = do 
      wasBetween <-detectMovement ( moveWhileE (betweenWord) GoRight)
      if wasBetween
         then return ()
         else do
            c <- readE
            moveWhileE (sameWord c) GoRight
            moveWhileE (betweenWord) GoRight

viewChar :: Action
viewChar = do
   c <- readE
   msgE . show $ c


--
-- movement commands
--
moveCmdFM :: [(Char, Int -> Action)]
moveCmdFM = 
-- left/right
    [('h',          left)
    ,('\^H',        left)
    ,(keyBackspace, left)
    ,('\BS',        left)
    ,('\127',       left)
    ,('l',          right)
    ,(' ',          right)
    ,(keyHome,      const solE)
    ,('^',          const firstNonSpaceE)
    ,('$',          const eolE)
    ,(keyEnd,       const eolE)
    ,('|',          \i -> solE >> rightOrEolE (i-1))

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
    ,('w',          \i -> replicateM_ i nextWord)
    ,('b',          \i -> replicateM_ i (do
         moved <- detectMovement begWord
         when (not moved) (leftE >> begWord)
         ))
    ,('e',          \i -> replicateM_ i (do
         moved <- detectMovement endWord
         when (not moved) (rightE >> endWord)
         ))



-- text
    ,('{',          prevNParagraphs)
    ,('}',          nextNParagraphs)

-- debuging
    ,('g',          \_ -> viewChar)

-- misc
    ,('H',          \i -> downFromTosE (i - 1))
    ,('M',          const middleE)
    ,('L',          \i -> upFromBosE (i - 1))

-- bogus entry
    ,('G',          const nopE)
    ]
    where
        left  i = leftOrSolE i
        right i = rightOrEolE i
        up    i = if i > 100 then gotoLnFromE (-i) else replicateM_ i upE
        down  i = if i > 100 then gotoLnFromE i    else replicateM_ i downE

--
-- more movement commands. these ones are paramaterised by a character
-- to find in the buffer.
--
move2CmdFM :: [(Char, Int -> Char -> Action)]
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
   choice [event c >> write (a i) | (c,a) <- cmdCmdFM ] +++
    (do event 'r'; c <- anyButEscOrDel; write (writeE c)) +++
    (events ">>" >> write (tabifySpacesOnLineAndShift i))+++
    (events "<<" >> write (tabifySpacesOnLineAndShift (-i)))+++
    (events "ZZ" >> write (viWrite >> quitE))

anyButEscOrDel :: VimProc Char
anyButEscOrDel = oneOf $ any' \\ ('\ESC':delete')


--
-- cmd mode commands
--
cmdCmdFM :: [(Char, Int -> Action)]
cmdCmdFM =
    [('\^B',    upScreensE)             -- vim does (firstNonSpaceE;leftOrSolE)
    ,('\^F',    downScreensE)
    ,('\^G',    const viFileInfo)        -- hmm. not working. duh. we clear
    ,('\^L',    const refreshE)
    ,('\^R',    flip replicateM_ redoE )
    ,('\^W',    const nextWinE)
    ,('\^Z',    const suspendE)
    ,('D',      const (readRestOfLnE >>= setRegE >> killE))
    ,('J',      const (eolE >> deleteE))    -- the "\n"
    ,('U',      flip replicateM_ undoE )    -- NB not correct
    ,('n',      const $ do getRegexE >>=
                               msgE . ("/" ++) . fst . fromMaybe ([],undefined)
                           searchE Nothing [] GoRight)
    ,('u',      flip replicateM_ undoE )

    ,('X',      \i -> do p <- getPointE
                         leftOrSolE i
                         q <- getPointE
                         when (p-q > 0) $ deleteNE (p-q) )

    ,('x',      \i -> do p <- getPointE -- not handling eol properly
                         rightOrEolE i
                         q <- getPointE
                         gotoPointE p
                         when (q-p > 0) $ deleteNE (q-p))

    ,('p',      (const $ rightOrEolE 1 >> getRegE >>= insertNE >> leftE))

    ,('P',      (const $ getRegE >>= insertNE >> leftE))

    ,(keyPPage, upScreensE)
    ,(keyNPage, downScreensE)
    ,(keyLeft,  leftOrSolE)
    ,(keyRight, rightOrEolE)
    ,('~',      \i -> do p <- getPointE
                         rightOrEolE i
                         q <- getPointE
                         gotoPointE p
                         mapRangeE p q $ \c ->
                             if isUpper c then toLower c else toUpper c
                         gotoPointE q)
    ]

--
-- | So-called 'operators', which take movement actions as arguments.
--
-- How do we achive this? We look for the known operator chars
-- (op_char), then parse one of the known movement commands.
-- We then apply the returned action 
-- and then the operator. For example, we 'd'
-- command stores the current point, does a movement, then deletes from
-- the old to the new point. FIXME: d is buggy!
cmd_op :: VimMode
cmd_op = do
  cnt <- count
  let i = maybe 1 id cnt
  choice $ [events "dd" >> write (solE >> killE >> deleteE),
            events "yy" >> write (readLnE >>= setRegE)] ++
           [do event c; m <- cmd_move; write (a i m) | (c,a) <- opCmdFM]
    where
        -- | operator (i.e. movement-parameterised) actions
        opCmdFM :: [(Char,Int -> Action -> Action)]
        opCmdFM =
            [('d', \i m -> replicateM_ i $ do
                              (p,q) <- withPointMove m
                              deleteNE (max 0 (abs (q - p) + 1))  -- inclusive
             ),
             ('y', \_ m -> do (p,q) <- withPointMove m
                              s <- (if p < q then readNM p q else readNM q p)
                              setRegE s -- ToDo registers not global.
             )]

        --
        -- A strange, useful action. Save the current point, move to
        -- some location specified by the sequence @m@, then return.
        -- Return the current, and remote point.
        --
        withPointMove :: Action -> EditorM (Int,Int)
        withPointMove m = do p <- getPointE
                             m
                             q <- getPointE
                             when (p < q) $ gotoPointE p
                             return (p,q)

--
-- | Switching to another mode from visual mode.
--
-- All visual commands are meta actions, as they transfer control to another
-- lexer. In this way vis_single is analogous to cmd2other
--
vis_single :: VimMode
vis_single =
        let beginIns a = do write (a >> unsetMarkE) >> get >>= ins
            yank = do mrk <- getMarkE
                      pt <- getPointE
                      readRegionE (mkVimRegion mrk pt) >>= setRegE
                      gotoPointE mrk
                      (mrkRow,_) <- getLineAndColE
                      gotoPointE pt
                      (ptRow,_) <- getLineAndColE
                      let rowsYanked = ptRow - mrkRow
                      if (rowsYanked > 2) then msgE ( (show rowsYanked) ++ " lines yanked")
                                          else nopE
            pasteOver = do mrk <- getMarkE
                           pt <- getPointE
                           text <- getRegE
                           gotoPointE mrk
                           deleteRegionE (mkVimRegion mrk pt)
                           insertNE text
            cut  = do mrk <- getMarkE
                      pt <- getPointE
                      (ptRow,_) <- getLineAndColE
                      readRegionE (mkVimRegion mrk pt) >>= setRegE
                      gotoPointE mrk
                      (mrkRow,_) <- getLineAndColE
                      deleteRegionE (mkVimRegion mrk pt)
                      let rowsCut = ptRow - mrkRow
                      if (rowsCut > 2) then msgE ( (show rowsCut) ++ " fewer lines")
                                       else nopE
        in choice [
            event '\ESC' >> write nopE,
            event 'v'    >> write nopE,
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
       shiftBy x = do mark <- getMarkE
                      (row2,_) <- getLineAndColE
                      gotoPointE mark
                      (row1,_) <- getLineAndColE
                      let step = if (row2 > row1) then downE
                                                  else upE
                          numOfLines = 1 + (abs (row2 - row1))
                      replicateM_ numOfLines (tabifySpacesOnLineAndShift x>>step)
   choice ([events "ZZ" >> write (viWrite >> quitE),
            events ">>" >> write (shiftBy i),
            events "<<" >> write (shiftBy (-i)),
            do event 'r'; x <- anyEvent; write $ do
                                   mrk <- getMarkE
                                   pt <- getPointE
                                   text <- readRegionE (mkVimRegion mrk pt)
                                   gotoPointE mrk
                                   deleteRegionE (mkVimRegion mrk pt)
                                   let convert '\n' = '\n'
                                       convert  _   = x
                                   insertNE . map convert $ text] ++
           [event c >> write (a i) | (c,a) <- cmdCmdFM ])


--
-- | Switch to another vim mode from command mode.
--
-- These commands are meta actions, as they transfer control to another
-- lexer. Some of these commands also perform an action before switching.
--
cmd2other :: VimMode
cmd2other = let beginIns a = write a >> get >>= ins
                beginIns :: Action -> VimMode
        in choice [
            do event ':'     ; ex_mode ":",
            do event 'v'     ; vis_mode,
            do event 'R'     ; rep_mode,
            do event 'i'     ; get >>= ins,
            do event 'I'     ; beginIns solE,
            do event 'a'     ; beginIns $ rightOrEolE 1,
            do event 'A'     ; beginIns eolE,
            do event 'o'     ; beginIns $ eolE >> insertE '\n',
            do event 'O'     ; beginIns $ solE >> insertE '\n' >> upE,
            do event 'c'     ; beginIns $ not_implemented 'c',
            do event 'C'     ; beginIns $ readRestOfLnE >>= setRegE >> killE,
            do event 'S'     ; beginIns $ solE >> readLnE >>= setRegE >> killE,
            do event '/'     ; ex_mode "/",
--          do event '?'   ; (with (not_implemented '?'), st{acc=[]}, Just $ cmd st),
            do event '\ESC'  ; write msgClrE >> get >>= cmdMode,
            do event keyIC   ; get >>= ins]


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
                    k | isDel k       -> do s <- atSofE
                                            unless s (leftE >> deleteE)
                      | k == keyPPage -> upScreenE
                      | k == keyNPage -> downScreenE
                      | k == keyUp    -> upE
                      | k == keyDown  -> downE
                      | k == keyLeft  -> leftE
                      | k == keyRight -> rightE
                      | k == keyEnd   -> eolE
                      | k == keyHome  -> solE
                    '\t' -> mapM_ insertE "    " -- XXX
                    _    -> insertE c

anyButEscOrCtlN :: VimProc Char
anyButEscOrCtlN = oneOf $ (keyBackspace : any' ++ cursc') \\ ['\ESC','\^N']

-- --------------------
-- | Keyword 
--
kwd_mode :: VimMode
kwd_mode = many1' (event '\^N' >> write wordCompleteE) >> write resetCompleteE


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
                    k | isDel k       -> leftE -- should undo unless pointer has been moved
                      | k == keyPPage -> upScreenE
                      | k == keyNPage -> downScreenE
                      | k == keyUp    -> upE
                      | k == keyDown  -> downE
                      | k == keyLeft  -> leftE
                      | k == keyRight -> rightE
                      | k == keyEnd   -> eolE
                      | k == keyHome  -> solE
                    '\t' -> mapM_ insertE "    " -- XXX
                    '\r' -> insertE '\n'
                    _ -> do e <- atEolE
                            if e then insertE c else writeE c >> rightE

-- ---------------------------------------------------------------------
-- Ex mode. We also lex regex searching mode here.

--
-- | ex mode is either accumulating input or, on \n, executing the command
--
ex_mode :: String -> VimMode
ex_mode s0 = do write cmdlineFocusE >> loop s0 >> write cmdlineUnFocusE
    where 
      loop s = do
        write (msgE s)
        choice [do c <- anyButDelNlArrow; ex_mode (s++[c]),
                do enter; ex_eval s,
                do event '\ESC'; return (),
                do delete; ex_mode (take (length s - 1) s),
                do event keyUp; histMove True >>= ex_mode,
                do event keyDown; histMove False >>= ex_mode]
      anyButDelNlArrow = oneOf $ any' \\ (enter' ++ delete' ++ ['\ESC',keyUp,keyDown])
                           
-- TODO when you go up, then down, you need 2 keypresses to go up again.
histMove :: Bool -> VimProc String
histMove up = do
  (h,i) <- return hist `ap` get
  let (s,i') = msg (h,i)
  modify $ \st -> st {hist=(h,i')}
  return s
      where
        msg (h,i)
               | null h = (":",0)
               | up     = if i < length h - 1
                           then (h !! i, i+1)
                           else (last h, length h - 1)
               | not up = if i > 0
                           then (h !! i, i-1)
                           else (head h, 0)
               | otherwise = error "the impossible happened"


--
-- eval an ex command to an Action, also appends to the ex history
--
ex_eval :: String -> VimMode
ex_eval cmd = do
  modify $ \st -> st {hist = (cmd:(fst $ hist st), snd $ hist st)}
  write $ case cmd of
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
          [] -> nopE

    where
      fn ""           = msgClrE

      fn s@(c:_) | isDigit c = do
        e <- lift $ try $ evaluate $ read s
        case e of Left _ -> errorE $ "The " ++show s++ " command is unknown."
                  Right lineNum -> gotoLnE lineNum

      fn "w"          = viWrite
      fn ('w':' ':f)  = viWriteTo f
      fn "q"          = do b <- isUnchangedE
                           if b then closeE
                                else errorE "No write since last change (add ! to override)"
      fn "q!"         = closeE
      fn "wq"         = viWrite >> closeE
      fn "n"          = nextBufW
      fn "$"          = botE
      fn "p"          = prevBufW
      fn ('s':'p':_)  = splitE
      fn ('e':' ':f)  = fnewE f
      fn ('s':'e':'t':' ':'f':'t':'=':ft)  = setSynE ft
      fn ('n':'e':'w':' ':f) = splitE >> fnewE f
      fn ('s':'/':cs) = viSub cs

      -- send just this line through external command /fn/
      fn ('.':'!':f) = do
            ln  <- readLnE
            ln' <- pipeE f ln
            solE
            killE
            mapM_ insertE ln' -- urgh.
            solE

--    Needs to occur in another buffer
--    fn ('!':f) = pipeE f []

      fn "reboot"     = rebootE     -- not in vim
      fn "reload"     = reloadE     -- not in vim

      fn "redr"       = refreshE
      fn "redraw"     = refreshE

      fn "u"          = undoE
      fn "undo"       = undoE
      fn "r"          = redoE
      fn "redo"       = redoE

      fn "sus"        = suspendE
      fn "suspend"    = suspendE
      fn "st"         = suspendE
      fn "stop"       = suspendE

      fn s            = errorE $ "The "++show s++ " command is unknown."


------------------------------------------------------------------------

not_implemented :: Char -> Action
not_implemented c = errorE $ "Not implemented: " ++ show c

-- ---------------------------------------------------------------------
-- Misc functions

viFileInfo :: Action
viFileInfo = 
    do bufInfo <- bufInfoE
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
viWrite :: Action
viWrite = do
    mf <- fileNameE
    case mf of
        Nothing -> errorE "no file name associate with buffer"
        Just f  -> do
            bufInfo <- bufInfoE
	    let s   = bufInfoFileName bufInfo
            let msg = msgE $ show f ++" "++show s ++ "C written"
            catchJust' ioErrors (fwriteToE f >> msg) (msgE . show)


-- | Try to write to a named file in the manner of vi\/vim
viWriteTo :: String -> Action
viWriteTo f = do
    let f' = (takeWhile (/= ' ') . dropWhile (== ' ')) f
    bufInfo <- bufInfoE
    let s   = bufInfoFileName bufInfo
    let msg = msgE $ show f'++" "++show s ++ "C written"
    catchJust' ioErrors (fwriteToE f' >> msg) (msgE . show)

-- | Try to do a substitution
viSub :: [Char] -> Action
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
cursc'   = [keyPPage, keyNPage, keyLeft, keyRight, keyDown, keyUp, keyHome, keyEnd]
