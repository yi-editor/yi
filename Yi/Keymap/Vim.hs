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
-- | Vim keymap for Yi. Emulates vim :set nocompatible
--

module Yi.Keymap.Vim ( keymap, keymapPlus, VimMode ) where

import Yi.Core
import Yi.Editor            ( Action )
import Yi.UI         hiding ( plus )
import Yi.Ctk.Lexers hiding ( Action )

import Prelude       hiding ( any )

import Data.Maybe           ( fromMaybe )
import Data.List            ( (\\) )
import Data.FiniteMap
import Data.Char            ( isUpper, toLower, toUpper, isSpace )

import Control.Monad        ( replicateM_, when )
import Control.Exception    ( ioErrors, catchJust )

-- ---------------------------------------------------------------------

-- A vim mode is a lexer that returns a core Action. 
type VimMode = Lexer  VimState Action 
type ViRegex = Regexp VimState Action

--
-- state threaded through the lexer
--
-- In vi, you may add bindings (:map) to cmd or insert mode. We thus
-- carry around the current cmd and insert lexers in the state. Calls to
-- switch editor modes therefore use the lexers in the state.
--
data VimState = 
        St { acc :: [Char]           -- an accumulator, for count, search and ex mode
           , hist:: ([String],Int)   -- ex-mode command history
           , cmd :: VimMode          -- (maybe augmented) cmd mode lexer
           , ins :: VimMode }        -- (maybe augmented) ins mode lexer

--
-- | Top level. Lazily consume all the input, generating a list of
-- actions, which then need to be forced
--
-- NB . if there is a (bad) exception, we'll lose any new bindings.. iorefs?
--    . also, maybe we shouldn't refresh automatically?
--
keymap :: [Char] -> [Action]
keymap cs = actions
    where 
        (actions,_,_) = execLexer cmd_mode (cs, defaultSt)

-- | default lexer state, just the normal cmd and insert mode. no mappings
defaultSt :: VimState
defaultSt = St { acc = [], hist = ([],0), cmd = cmd_mode, ins = ins_mode }

-- | like keymap, but takes a supplied lexer, which is used to augment the
-- existing lexer. Useful for user-added binds (to command mode only!)
keymapPlus :: VimMode -> [Char] -> [Action]
keymapPlus lexer' cs = actions
    where 
        actions = let (ts,_,_) = execLexer cmd_mode' (cs, dfltSt) in ts
        cmd_mode'= cmd_mode >||< lexer'
        dfltSt = St { acc = [], hist = ([],0), cmd = cmd_mode', ins = ins_mode }

------------------------------------------------------------------------

-- The vim lexer is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate lexers for each phase of key input in that mode.

-- | command mode consits of simple commands that take a count arg - the
-- count is stored in the lexer state. also the replace cmd, which
-- consumes one char of input, and commands that switch modes.
cmd_mode :: VimMode
cmd_mode = lex_count >||< cmd_eval >||< cmd_move >||< cmd2other >||< cmd_op

--
-- | insert mode is either insertion actions, or the meta \ESC action
--
ins_mode :: VimMode
ins_mode = ins_char >||< ins2cmd

--
-- | replace mode is like insert, except it performs writes, not inserts
--
rep_mode :: VimMode
rep_mode = rep_char >||< rep2cmd

--
-- | ex mode is either accumulating input or, on \n, executing the command
--
ex_mode :: VimMode
ex_mode = ex_char >||< ex_edit >||< ex_hist >||< ex_eval >||< ex2cmd

-- util
with :: Action -> Maybe (Either e Action)
with a = (Just (Right a))

------------------------------------------------------------------------
--
-- A lexer to accumulate digits, echoing them to the cmd buffer. This is
-- typically what is needed for integer repetition arguments to commands
--
-- ToDo don't handle 0 properly
--
lex_count :: VimMode
lex_count = digit
    `meta` \[c] st -> (with (msg (c:acc st)),st{acc = c:acc st},Just $ cmd st)
    where
        msg cs = msgE $ (replicate 60 ' ') ++ (reverse cs)

-- ---------------------------------------------------------------------
-- | Movement commands

cmd_move :: VimMode
cmd_move = move_chr
    `meta` \[c] st@St{acc=count} -> 
        let cnt= if null count then Nothing else Just (read $ reverse count)
            i  = fromMaybe 1 cnt
            fn = getCmd c i
        in (with (msgClrE >> fn), st{acc=[]}, Just $ cmd st)
    where
        move_chr   = alt $ keysFM moveCmdFM
        getCmd c i = let mfn = lookupFM moveCmdFM c
                     in flip ($) i (fromMaybe (const nopE) mfn)

--
-- movement command
--
moveCmdFM :: FiniteMap Char (Int -> Action)
moveCmdFM = listToFM $

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
    ,('|',          const solE)

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
-- text
-- misc
    ,('H',          \i -> downFromTosE (i - 1))
    ,('M',          const middleE)
    ,('L',          \i -> upFromBosE (i - 1))
    ]
    where
        left  i = leftOrSolE i
        right i = rightOrEolE i
        up    i = if i > 100 then gotoLnFromE (-i) else replicateM_ i upE
        down  i = if i > 100 then gotoLnFromE i    else replicateM_ i downE

------------------------------------------------------------------------
--
-- Other command mode functions
--
cmd_eval :: VimMode
cmd_eval = (cmd_char >|< 
            (char 'r' +> anyButEscOrDel) >|<
            string ">>" >|< string "<<" >|< 
            string "ZZ" >|< string "yy")

    `meta` \lexeme st@St{acc=count} -> 
        let cnt= if null count then Nothing else Just (read $ reverse count)
            i  = fromMaybe 1 cnt
            fn = case lexeme of
                    "ZZ"    -> viWrite >> quitE
                    -- todo: fix the unnec. refreshes that happen
                    ">>"    -> do replicateM_ i $ solE >> mapM_ insertE "    " 
                                  firstNonSpaceE
                    "<<"    -> do solE
                                  replicateM_ i $
                                    replicateM_ 4 $
                                        readE >>= \k -> 
                                            when (isSpace k) deleteE 
                                  firstNonSpaceE
                    "yy"    -> readLnE >>= setRegE
                    'r':[x] -> writeE x
                    [c]     -> getCmd c i
                    _       -> undef (head lexeme)

        in (with (msgClrE >> fn), st{acc=[]}, Just $ cmd st)

    where
        anyButEscOrDel = alt $ any' \\ ('\ESC':delete')
        cmd_char       = alt $ keysFM cmdCmdFM

        getCmd c i = let mfn = lookupFM cmdCmdFM c
                     in flip ($) i (fromMaybe (const $ undef c) mfn)
                    
        undef c = not_implemented c

--
-- cmd mode commands
--
cmdCmdFM :: FiniteMap Char (Int -> Action)
cmdCmdFM = listToFM $
    [('\^B',    upScreensE)
    ,('\^F',    downScreensE)
    ,('\^G',    const viFileInfo)        -- hmm. not working. duh. we clear
    ,('\^W',    const nextWinE)
    ,('D',      const (readRestOfLnE >>= setRegE >> killE))
    ,('J',      const (eolE >> deleteE))    -- the "\n"
    ,('X',      (\i -> leftOrSolE i >> replicateM_ i deleteE))
    ,('n',      const (searchE Nothing))
    ,('x',      deleteNE)
    ,('~',      (\i -> 
                 let fn = do c' <- readE
                             let c'' = if isUpper c' 
                                       then toLower c' 
                                       else toUpper c'
                             writeE c''
                             rightE
                 in replicateM_ i fn))

    -- hmm. 'G' commands need a bit more work
--  ,('G',      (case c of Nothing -> botE >> solE
--                         Just n  -> gotoLnE n))

    ,('p',      (\_ -> getRegE >>= \s -> 
                        eolE >> insertE '\n' >> 
                            mapM_ insertE s >> solE))
    ,(keyPPage, upScreensE)
    ,(keyNPage, downScreensE)
    ,(keyLeft,  const leftE)          -- not really vi, but fun
    ,(keyRight, const rightE)
    ]

------------------------------------------------------------------------
--
-- | So-called 'operators', which take movement actions as arguments.
--
-- An alternative lexer is needed for text-change characters that take
-- movement commands as arguments. Like so:
--      d2w
--
-- op-semantics: p <- point ; move cursor 2 words. q <- point; deleteN p q
--
-- remember. 'd' could have arguments.
--
-- So, if followed by a move_char, just execute the movement, then
-- perform on range from p to new p.
--
-- Lots ToDo yet. Perhaps a special lexer just for accumulating more
-- numbers and characters. Or a flag in the state (urgh)
--
-- command and motion keys should be broken into different lexers.
-- need to have a motion mode, that includes the int repeat lexer.
--
cmd_op :: VimMode
cmd_op = (op_char +> move_chr >|< string "dd")
    `meta` \lexeme st@St{acc=count} -> 
        let c  = if null count then Nothing else Just (read $ reverse count)
            i  = fromMaybe 1 c
            fn = getCmd lexeme c i
        in (with (msgClrE >> fn), st{acc=[]}, Just $ cmd st)
    where
        op_char  = alt "d"             -- just for now
        move_chr = alt $ keysFM moveCmdFM

        getCmd :: [Char] -> (Maybe Int) -> Int -> Action
        getCmd lexeme _c i = case lexeme of
            -- shortcuts
            "dd" -> solE >> killE >> deleteE

            -- todo: yank
            ('d':m:[]) -> replicateM_ i $ do
                               p <- getPointE
                               foldr (>>) nopE (getMove m)
                               q <- getPointE
                               when (p < q) $ gotoPointE p
                               deleteNE (abs (q - p))
            _ -> nopE

        -- no idea if this state is right, yet.
        -- movement command to perform .. ToDo should be (cmd st)
        getMove c = let (as,_,_) = execLexer cmd_move ([c], defaultSt) in as

--
-- | Switch to another vim mode.
--
-- These commands are meta actions, as they transfer control to another
-- lexer. Some of these commands also perform an action before switching.
--
cmd2other :: VimMode
cmd2other = modeSwitchChar
    `meta` \[c] st -> 
        let beginIns a = (with a, st, Just (ins st))
        in case c of
            ':' -> (with (msgE ":"), st{acc=[':']}, Just ex_mode)
            'R' -> (Nothing, st, Just rep_mode)
            'i' -> (Nothing, st, Just (ins st))
            'I' -> beginIns solE
            'a' -> beginIns $ rightOrEolE 1
            'A' -> beginIns eolE
            'o' -> beginIns $ eolE >> insertE '\n'
            'O' -> beginIns $ solE >> insertE '\n' >> upE
            'c' -> beginIns $ not_implemented 'c'
            'C' -> beginIns $ readRestOfLnE >>= setRegE >> killE
            'S' -> beginIns $ solE >> readLnE >>= setRegE >> killE

            '/' -> (with (msgE "/"), st{acc=['/']}, Just ex_mode)
            '?' -> (with (not_implemented '?'), st{acc=[]}, Just $ cmd st)

            '\ESC'-> (with msgClrE, st{acc=[]}, Just $ cmd st)

            s   -> (with (errorE ("The "++show s++" command is unknown."))
                   ,st, Just $ cmd st)

    where modeSwitchChar = alt ":RiIaAoOcCS/?\ESC"

-- ---------------------------------------------------------------------
-- | vim insert mode
-- 
ins_char :: VimMode
ins_char = anyButEsc
    `action` \[c] -> Just (fn c)

    where fn c = case c of
                    k | isDel k       -> leftE >> deleteE
                      | k == keyPPage -> upScreenE
                      | k == keyNPage -> downScreenE
                      | k == '\t'     -> mapM_ insertE "    " -- XXX
                    _ -> insertE c

          anyButEsc = alt $ (keyBackspace : any' ++ cursc') \\ ['\ESC']

-- switch out of ins_mode
ins2cmd :: VimMode
ins2cmd  = char '\ESC' `meta` \_ st -> (with (leftOrSolE 1), st, Just $ cmd st)

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
rep_char = anyButEsc
    `action` \[c] -> Just (fn c)
    where fn c = case c of
                    k | isDel k       -> leftE >> deleteE 
                      | k == keyPPage -> upScreenE
                      | k == keyNPage -> downScreenE
                    '\t' -> mapM_ insertE "    " -- XXX
                    '\r' -> insertE '\n' 
                    _ -> do e <- atEolE
                            if e then insertE c else writeE c >> rightE

          anyButEsc = alt $ (keyBackspace : any' ++ cursc') \\ ['\ESC']

-- switch out of rep_mode
rep2cmd :: VimMode
rep2cmd  = char '\ESC' `meta` \_ st -> (Nothing, st, Just $ cmd st)

-- ---------------------------------------------------------------------
-- Ex mode. We also lex regex searching mode here.

-- normal input to ex. accumlate chars
ex_char :: VimMode
ex_char = anyButDelNlArrow
    `meta` \[c] st -> (with (msg c), st{acc=c:acc st}, Just ex_mode)
    where
        anyButDelNlArrow = alt $ any' \\ (enter' ++ delete' ++ ['\ESC',keyUp,keyDown])
        msg c = getMsgE >>= \s -> msgE (s++[c])

-- history editing
-- TODO when you go up, then down, you need 2 keypresses to go up again.
ex_hist :: VimMode
ex_hist = arrow
    `meta` \[key] st@St{hist=(h,i)} -> 
                let (s,i') = msg key (h,i)
                in (with (msgE s),st{acc=reverse s,hist=(h,i')}, Just ex_mode)
    where
        msg :: Char -> ([String],Int) -> (String,Int)
        msg key (h,i) = case () of {_
                | null h         -> (":",0)
                | key == keyUp   -> if i < length h - 1 
                                    then (h !! i, i+1)
                                    else (last h, length h - 1)
                | key == keyDown -> if i > 0 
                                    then (h !! i, i-1)
                                    else (head h, 0)
                | otherwise      -> error "ex_hist: the impossible happened"
            }

        arrow = alt [keyUp, keyDown]

-- line editing
ex_edit :: VimMode
ex_edit = delete
    `meta` \_ st -> 
        let cs' = case acc st of [c]    -> [c]
                                 (_:xs) -> xs
                                 []     -> [':'] -- can't happen
        in (with (msgE (reverse cs')), st{acc=cs'}, Just ex_mode)

-- escape exits ex mode immediately
ex2cmd :: VimMode
ex2cmd = char '\ESC'
    `meta` \_ st -> (with msgClrE, st{acc=[]}, Just $ cmd st)

--
-- eval an ex command to an Action, also appends to the ex history
--
ex_eval :: VimMode
ex_eval = enter
    `meta` \_ st@St{acc=dmc} -> 
        let c  = reverse dmc
            h  = (c:(fst $ hist st), snd $ hist st) in case c of
        -- regex searching
        ('/':pat) -> (with (searchE (Just pat))
                     ,st{acc=[],hist=h},Just $ cmd st)

        -- add mapping to command mode
        (_:'m':'a':'p':' ':cs) -> 
               let pair = break (== ' ') cs
                   cmd' = uncurry (eval_map st (Left $ cmd st)) pair
               in (with msgClrE, st{acc=[],hist=h,cmd=cmd'}, Just cmd')

        -- add mapping to insert mode
        (_:'m':'a':'p':'!':' ':cs) -> 
               let pair = break (== ' ') cs
                   ins' = uncurry (eval_map st (Right $ ins st)) pair
               in (with msgClrE, st{acc=[],hist=h,ins=ins'}, Just (cmd st))

        -- unmap a binding from command mode
        (_:'u':'n':'m':'a':'p':' ':cs) ->
               let cmd' = eval_unmap (Left $ cmd st) cs
               in (with msgClrE, st{acc=[],hist=h,cmd=cmd'}, Just cmd')

        -- unmap a binding from insert mode
        (_:'u':'n':'m':'a':'p':'!':' ':cs) ->
               let ins' = eval_unmap (Right $ ins st) cs
               in (Nothing, st{acc=[],hist=h,ins=ins'}, Just (cmd st))

        -- just a normal ex command
        (_:src) -> (with (fn src), st{acc=[],hist=h}, Just $ cmd st)

        -- can't happen, but deal with it
        [] -> (Nothing, st{acc=[], hist=h}, Just $ cmd st)

    where 
      fn ""           = msgClrE
      fn "w"          = viWrite
      fn "q"          = closeE
      fn "q!"         = closeE
      fn "wq"         = viWrite >> closeE
      fn "n"          = nextBufW
      fn "p"          = prevBufW
      fn ('s':'p':_)  = splitE
      fn ('e':' ':f)  = fnewE f
      fn ('s':'/':cs) = viSub cs
      fn s            = errorE $ "The "++show s++ " command is unknown."

------------------------------------------------------------------------
--
-- | Given a lexer, a sequence of chars, and another sequence of
-- chars, bind the lhs to the actions produced by the rhs, and augment
-- the lexer with the new binding. (Left == command mode)
--
eval_map :: VimState                 -- ^ current state (including other lexers)
         -> (Either VimMode VimMode)  -- ^ which mode we're tweaking
         -> [Char]                  -- ^ identifier to bind to
         -> [Char]                  -- ^ body of macro, to be expanded
         -> VimMode                  -- ^ resulting augmented mode

eval_map st emode lhs rhs = mode >||< bind
    where
        mode     = either id id emode
        st'      = case emode of
                        Left  cmd' -> st {acc=[], cmd=cmd'}
                        Right ins' -> st {acc=[], ins=ins'}
        (as,_,_) = execLexer mode (rhs, st')
        bind     = string lhs `action` \_ -> Just (foldl (>>) nopE as)

--
-- | Unmap a binding from the keymap (i.e. nopE a lex table entry)
-- Currently we unmap all the way back to the original mode. So you
-- can't stack bindings. This is vi's behaviour too, roughly.
--
eval_unmap :: (Either VimMode VimMode)  -- ^ which vim mode to unmap from
           -> [Char]                  -- ^ identifier to unmap
           -> VimMode                  -- ^ new, depleted mode

eval_unmap emode lhs = mode >||< bind
    where
        mode      = either id id emode
        dflt_mode = either (const cmd_mode) (const ins_mode) emode
        (as,_,_) = execLexer dflt_mode (lhs, defaultSt)
        bind     = case as of
                    [] -> string lhs `action` \_ -> Just nopE -- wasn't bound prior
                    [a]-> string lhs `action` \_ -> Just a    -- bound to just one
                    _  -> string lhs `action` \_ -> Just nopE 
                            -- components of the command were bound. too hard

------------------------------------------------------------------------

not_implemented :: Char -> Action
not_implemented c = errorE $ "Not implemented: " ++ show c

-- ---------------------------------------------------------------------
-- Misc functions

viFileInfo :: Action
viFileInfo = do (f,_,ln,_,_,pct) <- bufInfoE 
                msgE $ show f ++ " Line " ++ show ln ++ " ["++ pct ++"]"

-- | Try to write a file in the manner of vi\/vim
-- Need to catch any exception to avoid losing bindings
viWrite :: Action
viWrite = do
    (f,s,_,_,_,_) <- bufInfoE 
    let msg = msgE $ show f++" "++show s ++ "C written"
    catchJust ioErrors (fwriteE >> msg) (msgE . show)

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
digit, delete, enter :: ViRegex
digit   = alt digit'
enter   = alt enter'
delete  = alt delete'
-- any     = alt any'

enter', any', digit', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']
digit'   = ['0' .. '9']

cursc' :: [Char]
cursc'   = [keyPPage, keyNPage, keyLeft, keyRight, keyDown, keyUp]

