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
-- Vi-ish keymap for Yi
--

module Yi.Keymap.Vi ( keymap ) where

import Yi.Core
import Yi.UI         hiding ( plus )
import Yi.Ctk.Lexers hiding ( Action )

import Prelude hiding   ( any )

import Data.Maybe
import Data.List        ( (\\) )
import Data.Char
import Control.Monad

-- ---------------------------------------------------------------------
-- A vi mode is a lexer that returns a core Action. 
-- It can carry around some state, if it needs to.
--
type ViState = [Char]      -- state is key accumulator
type ViMode  = Lexer  ViState Action 
type ViRegex = Regexp ViState Action

-- ---------------------------------------------------------------------
--
-- | Top level. Lazily consume all the input, generating a list of
-- actions, which then need to be forced
--
-- NB if there is an exception, we'll lose any new bindings..
--    maybe we shouldn't refresh automatically?
--
keymap :: String -> IO ()
keymap cs = mapM_ (>> refreshE) actions
    where 
        actions = let (ts,_,_) = execLexer lexer (cs, emptySt) in ts
        emptySt = []

------------------------------------------------------------------------

-- The vi lexer is broken up into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate lexers for each phase of key input in that mode.

-- Top level lexer. We start in command mode
lexer :: ViMode
lexer = cmd_mode

-- command mode consits of simple commands that take a count arg - the
-- count is stored in the lexer state. also the replace cmd, which
-- consumes one char of input, and commands that switch modes.
cmd_mode :: ViMode
cmd_mode = cmd_count >||< cmd_eval >||< cmd2other

--
-- insert mode is either insertion actions, or the meta \ESC action
--
ins_mode :: ViMode
ins_mode = ins >||< ins2cmd

--
-- ex mode is either accumulating input or, on \n, executing the command
--
ex_mode :: ViMode
ex_mode = ex_char >||< ex_edit >||< ex_eval >||< ex2cmd

-- ---------------------------------------------------------------------
-- | vi (simple) command mode.
--

-- accumulate count digits, echoing them to the cmd buffer
cmd_count :: ViMode
cmd_count = digit
    `meta` \[c] acc -> (Just (Right $ msg (c:acc)), (c:acc), Just cmd_mode) 
    where
        msg cs = msgE $ (replicate 60 ' ') ++ (reverse cs)

-- eval a cmd 
cmd_eval :: ViMode
cmd_eval = ( cmdc >|< 
            (char 'r' +> anyButEscOrDel) >|<
            string ">>" >|< string "dd" >|< string "ZZ" >|< string "yy")

    `meta` \lexeme count -> 
        let c  = if null count then Nothing 
                               else Just (read $ reverse count)
            i  = fromMaybe 1 c
            fn = getCmd lexeme c i
        in (Just (Right fn), [], Just cmd_mode)

    where
        anyButEscOrDel = alt $ any' \\ ('\ESC':delete')

        -- command chars to actions
        getCmd :: [Char] -> (Maybe Int) -> Int -> Action
        getCmd lexeme c i = case lexeme of
            "\^A" -> undef
            "\^B" -> upScreensE i
            "\^D" -> undef
            "\^E" -> undef
            "\^F" -> downScreensE i
            "\^G" -> viFileInfo
            "\^H" -> leftOrSolE  i
            "\^J" -> replicateM_ i downE
            "\^L" -> undef
            "\^M" -> undef
            "\^N" -> replicateM_ i downE
            "\^P" -> replicateM_ i upE
            "\^R" -> undef
            "\^T" -> undef
            "\^U" -> undef
            "\^W" -> nextWinE
            "\^Y" -> undef
            "\^Z" -> undef
            "\^]" -> undef
            "\^^" -> undef
            " "   -> rightOrEolE i
            "!"   -> undef
            "#"   -> undef
            "$"   -> eolE
            "%"   -> undef
            "&"   -> undef
            "("   -> undef
            ")"   -> undef
            "+"   -> undef
            ","   -> undef
            "-"   -> undef
            "."   -> undef
      --    "0"   -> solE   -- don't want this. clashes with count
            "|"   -> solE
            ";"   -> undef
            "<"   -> undef
            "?"   -> undef
            "@"   -> undef
            "~"   -> do c' <- readE
                        let c'' = if isUpper c' then toLower c' else toUpper c'
                        writeE c''
            "B"   -> undef
            "D"   -> readRestOfLnE >>= setRegE >> killE
            "E"   -> undef
            "F"   -> undef
            "G"   -> case c of Nothing -> botE; Just n  -> gotoLnE n
            "N"   -> undef
            "H"   -> downFromTosE (i - 1)
            "J"   -> eolE >> deleteE -- the "\n"
            "L"   -> upFromBosE (i - 1)
            "M"   -> middleE
            "P"   -> undef
            "Q"   -> undef
            "R"   -> undef
            "T"   -> undef
            "U"   -> undef
            "W"   -> undef
            "X"   -> leftOrSolE i >> replicateM_ i deleteE
            "Y"   -> undef
            "h"   -> leftOrSolE  i
            "j"   -> replicateM_ i downE
            "k"   -> replicateM_ i upE
            "l"   -> rightOrEolE i
            "n"   -> searchE Nothing
            "p"   -> do s <- getRegE
                        eolE >> insertE '\n' >> mapM_ insertE s >> solE
            "q"   -> quitE
            "x"   -> replicateM_ i deleteE
            "ZZ"  -> viWrite >> quitE
            "dd"  -> solE >> killE >> deleteE
            ">>"  -> replicateM_ i $ solE >> mapM_ insertE "    " 
            "yy"  -> readLnE >>= setRegE

            'r':[x] -> writeE x

            [k] | k == keyPPage -> upScreensE i
                | k == keyNPage -> downScreensE i
                | k == keyLeft  -> leftOrSolE i
                | k == keyDown  -> replicateM_ i downE
                | k == keyUp    -> replicateM_ i upE

            _ -> undef

            where undef = not_implemented (head lexeme)

--
-- | Switch to another vi mode. 
--
-- These commands are meta actions, as they transfer control to another
-- lexer. Some of these commands also perform an action before switching.
--
cmd2other :: ViMode
cmd2other = modeSwitchChar
    `meta` \[c] st -> 
        let beginIns a = (Just (Right (a)), st, Just ins_mode)
        in case c of
            ':' -> (Just (Right (msgE ":")), [':'], Just ex_mode)

            'i' -> (Nothing, st, Just ins_mode)
            'I' -> beginIns solE
            'a' -> beginIns $ rightOrEolE 1
            'A' -> beginIns eolE
            'o' -> beginIns $ eolE >> insertE '\n'
            'O' -> beginIns $ solE >> insertE '\n' >> upE
            'c' -> beginIns $ not_implemented 'c'
            'C' -> beginIns $ readRestOfLnE >>= setRegE >> killE
            'S' -> beginIns $ solE >> readLnE >>= setRegE >> killE

            '/' -> (Just (Right (msgE "/")), ['/'], Just ex_mode)
            '?' -> beginIns $ not_implemented '?'

            '\ESC'-> (Just (Right msgClrE), [], Just cmd_mode)

            s   -> (Just (Right (
                        msgE ("The "++show s++" command is unknown.")))
                    ,st, Just cmd_mode)

    where modeSwitchChar = alt ":iIaAoOcCS/?\ESC"

-- ---------------------------------------------------------------------
-- | vi insert mode
-- 
ins :: ViMode
ins = anyButEsc
    `action` \[c] -> Just $ case c of
        k | isDel k       -> deleteE
          | k == keyPPage -> upScreenE
          | k == keyNPage -> downScreenE

        _ -> do (_,s,_,_,_,_) <- bufInfoE
                when (s == 0) $ insertE '\n' -- vi behaviour at start of file
                insertE c

    where anyButEsc = alt $ any' \\ ['\ESC']

-- switching out of ins_mode
ins2cmd :: ViMode
ins2cmd  = char '\ESC' 
    `meta` \_ rest -> (Nothing, rest, Just cmd_mode)

-- ---------------------------------------------------------------------
-- Ex mode. We also lex regex searching mode here.

-- normal input to ex. accumlate chars
ex_char :: ViMode
ex_char = anyButDelOrNl
    `meta` \[c] acc -> (Just (Right $ msg c), (c:acc), Just ex_mode)
    where
        anyButDelOrNl= alt $ any' \\ (enter' ++ delete')
        msg c = getMsgE >>= \s -> msgE (s++[c])

-- line editing
ex_edit :: ViMode
ex_edit = delete
    `meta` \_ cs -> 
        let acc = case cs of [c]    -> [c]
                             (_:xs) -> xs
                             []     -> [':'] -- can't happen
        in (Just (Right $ msgE (reverse acc)), acc, Just ex_mode)

-- escape exits ex mode immediately
ex2cmd :: ViMode
ex2cmd = char '\ESC'
    `meta` \_ _ -> (Just (Right msgClrE), [], Just cmd_mode)

ex_eval :: ViMode
ex_eval = enter
    `meta` \_ dmc -> case reverse dmc of

        -- regex searching
        ('/':pat) -> (Just (Right (searchE (Just pat))), [], Just cmd_mode)

        -- new key bindings. for now, just add bindings to echo n.b we
        -- haven't deleted the old binding, so you better now _rebind_
        -- keys. but only bind new ones, until we fix Lexers.hs
        (_:'l':'e':'t':' ':[c]) ->
               (Just (Right (bindmsg c)), [], Just (cmd_mode >||< new c))

        (_:cmd) -> (Just (Right (fn cmd)), [], Just cmd_mode)

        [] -> (Nothing, [], Just cmd_mode) -- can't happen

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
      fn ('s':'/':cs) = viSub cs >> msgClrE
      fn s            = msgE $ "The "++show s++ " command is unknown."

      -- generate a new lexer binding 'c' to silly echo function
      new c = char c `action` \_ -> Just $ msgE $ "echo " ++ show c

      bindmsg c = msgE $ show c ++ "is now bound to msgE()"

------------------------------------------------------------------------

not_implemented :: Char -> Action
not_implemented c = msgE $ "Not implemented: " ++ show c

-- ---------------------------------------------------------------------
-- Misc functions

viFileInfo :: Action
viFileInfo = do (f,_,ln,_,_,pct) <- bufInfoE 
                msgE $ show f ++ " Line " ++ show ln ++ " ["++ pct ++"]"

-- | Try and write a file in the manner of vi\/vim
viWrite :: Action
viWrite = do
    (f,s,_,_,_,_) <- bufInfoE 
    fwriteE
    msgE $ show f++" "++show s ++ "C written"

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
        ['g'] -> searchAndRepLocal pat rep  -- TODO
        _     -> searchAndRepLocal pat rep

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

-- ---------------------------------------------------------------------
-- | simple command mode keys (ones that don't consume input, or switch modes)
cmdc :: ViRegex
cmdc     = alt cmdc'

cmdc' :: [Char]
cmdc'    = cmdctrl' ++ cursc' ++ special' ++ upper' ++ lower'

special', upper', lower', cmdctrl', cursc' :: [Char]
special' = " !#$%()+,-.|;<?@~"
upper'   = "BDEFGNHJLMPQRTUWXY"
lower'   = "hjklnpqx"
cmdctrl' = ['\^A','\^B','\^D','\^E','\^F','\^H','\^J','\^L','\^M','\^N',
            '\^P','\^R','\^T','\^U','\^W','\^Y','\^Z','\ESC','\^]','\^^']
cursc'   = [keyPPage, keyNPage, keyLeft, keyDown, keyUp]

