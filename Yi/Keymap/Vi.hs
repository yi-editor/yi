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

--
-- | Top level. Lazily consume all the input, generating a list of
-- actions, which then need to be forced
--
-- NB if there is an exception, we'll lose any new bindings..
--
keymap :: String -> IO ()
keymap cs =
    let (ts,_,_) = execLexer lexer (cs,(){-state-})
    in mapM_ (>> refreshE) ts

-- | start in command mode (which includes the lexers for regex and search).
lexer :: Lexer () Action
lexer = cmd_mode

--
-- command mode consits of simple commands, two commands that consume
-- input, and commands that switch modes.
--
cmd_mode :: Lexer () Action
cmd_mode = simplecmd >||< replace >||< search >||< cmd2other

--
-- insert mode is either insertion actions, or the meta \ESC action
--
ins_mode :: Lexer () Action
ins_mode = ins >||< ins2cmd

-- ---------------------------------------------------------------------
-- | vi (simple) command mode. search mode is considered separately, as
-- it consumes other input.
--
simplecmd :: Lexer () Action
simplecmd = digit `star` (cmdc >|< string ">>" 
                               >|< string "dd"
                               >|< string "ZZ" 
                               >|< string "yy")
    `action` \lexeme -> Just $
        let (b,cs) = span isDigit lexeme
            c = if null b then Nothing else Just (read b)
            i = fromMaybe 1 c
        in case cs of
            "\^A" -> not_implemented (head cs)
            "\^B" -> upScreensE i
            "\^D" -> not_implemented (head cs)
            "\^E" -> not_implemented (head cs)
            "\^F" -> downScreensE i
            "\^G" -> viFileInfo
            "\^H" -> leftOrSolE  i
            "\^J" -> replicateM_ i downE
            "\^L" -> not_implemented (head cs)
            "\^M" -> not_implemented (head cs)
            "\^N" -> replicateM_ i downE
            "\^P" -> replicateM_ i upE
            "\^R" -> not_implemented (head cs)
            "\^T" -> not_implemented (head cs)
            "\^U" -> not_implemented (head cs)
            "\^W" -> nextWinE
            "\^Y" -> not_implemented (head cs)
            "\^Z" -> not_implemented (head cs)
            "\ESC"-> not_implemented (head cs)
            "\^]" -> not_implemented (head cs)
            "\^^" -> not_implemented (head cs)

            " "   -> rightOrEolE i
            "!"   -> not_implemented (head cs)
            "#"   -> not_implemented (head cs)
            "$"   -> eolE
            "%"   -> not_implemented (head cs)
            "&"   -> not_implemented (head cs)
            "("   -> not_implemented (head cs)
            ")"   -> not_implemented (head cs)
            "+"   -> not_implemented (head cs)
            ","   -> not_implemented (head cs)
            "-"   -> not_implemented (head cs)
            "."   -> not_implemented (head cs) -- last command could be in state
            "0"   -> solE
            "|"   -> solE
            ";"   -> not_implemented (head cs)
            "<"   -> not_implemented (head cs)
            "?"   -> not_implemented (head cs)
            "@"   -> not_implemented (head cs)
            "~"   -> do c' <- readE
                        let c'' = if isUpper c' then toLower c' else toUpper c'
                        writeE c''

            "B"   -> not_implemented (head cs)
            "D"   -> readRestOfLnE >>= setRegE >> killE
            "E"   -> not_implemented (head cs)
            "F"   -> not_implemented (head cs)
            "G"   -> case c of 
                        Nothing -> botE
                        Just n  -> gotoLnE n

            "N"   -> not_implemented (head cs)
            "H"   -> downFromTosE (i - 1)
            "J"   -> eolE >> deleteE -- the "\n"
            "L"   -> upFromBosE (i - 1)
            "M"   -> middleE
            "P"   -> not_implemented (head cs)
            "Q"   -> not_implemented (head cs)
            "R"   -> not_implemented (head cs)
            "T"   -> not_implemented (head cs)
            "U"   -> not_implemented (head cs)
            "W"   -> not_implemented (head cs)
            "X"   -> leftOrSolE i >> replicateM_ i deleteE
            "Y"   -> not_implemented (head cs)

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

            [k] | k == keyPPage -> upScreensE i   -- ? hmm.
                | k == keyNPage -> downScreensE i
                | k == keyLeft  -> leftOrSolE i
                | k == keyDown  -> replicateM_ i downE
                | k == keyUp    -> replicateM_ i upE

            k   -> not_implemented (head k)

-- | Replace a single char. Consumes more input
replace :: Lexer () Action
replace = char 'r' +> any `action` \(_:c:[]) -> Just $ writeE c

-- | Searching
-- Should make this a full lexer mode of its own, so as to echo chars
search :: Lexer () Action
search = char '/' +> anyButEnter `star` enter
    `action` \lexeme -> Just $
        let pat' = init (tail lexeme)
            pat  = clean pat'
        in msgE ('/':pat) >> searchE (Just pat)

--
-- | Switch to another vi mode. 
--
-- These commands are meta actions, as they transfer control to another
-- lexer. Some of these commands also perform an action before switching.
--
cmd2other :: Lexer () Action
cmd2other = 
    (char ':' >|< char 'i' >|< char 'I' >|< char 'a' >|< char 'A' >|< 
     char 'o' >|< char 'O' >|< char 'c' >|< char 'C' >|< char 'S')
    `meta` \[c] rest -> 
        let beginIns a = (Just (Right (a)), rest, Just ins_mode)
        in case c of
            ':' -> (Just (Right (msgE ":")), rest, Just ex_mode)
            'i' -> (Nothing, rest, Just ins_mode)
            'I' -> beginIns solE
            'a' -> beginIns $ rightOrEolE 1
            'A' -> beginIns eolE
            'o' -> beginIns $ eolE >> insertE '\n'
            'O' -> beginIns $ solE >> insertE '\n' >> upE
            'c' -> beginIns $ not_implemented 'c'
            'C' -> beginIns $ readRestOfLnE >>= setRegE >> killE
            'S' -> beginIns $ solE >> readLnE >>= setRegE >> killE
            s   -> (Just(Right(msgE ("The "++show s++" command is unknown.")))
                    ,rest, Just cmd_mode)

-- ---------------------------------------------------------------------
-- | vi insert mode
-- 
ins :: Lexer () Action
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
ins2cmd :: Lexer () Action
ins2cmd  = char '\ESC' `meta` \_ rest -> (Nothing, rest, Just cmd_mode)

-- ---------------------------------------------------------------------
-- | todo, perhaps we could set an editor flag to echo each char with
-- msg as it is read?
--
-- ex mode immediately switches back to command mode
--
-- NB doesn't echo input! Fix me
--
ex_mode :: Lexer () Action
ex_mode = anyButEnter `star` enter
    `meta` \cs rest -> case cs of
        ('l':'e':'t':' ':c:_) 
            -> -- for now, just add bindings to echo
               -- n.b we haven't deletted the old binding,
               -- so you better now _rebind_ keys. but only
               -- bind new ones, until we fix Lexers.hs
             (Just (Right (msgE $ show c ++ " bound to msgE")), 
                            rest, Just (cmd_mode >||< new c))

        _ -> (Just (Right (fn $ init cs)), rest, Just cmd_mode)
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
      fn s            = msgE $ "The "++show s++ " command is unknown."

      -- generate a new lexer binding 'c' to silly echo function
      new c = char c `action` \_ -> 
            Just (msgE $ show c ++ " bound to the msg function")

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
clean :: [Char] -> [Char]
clean (_:c:cs) | isDel c = clean cs
clean (c:cs)   | isDel c = clean cs
clean (c:cs) = c : clean cs
clean [] = []

-- | Is a delete sequence
isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False

-- ---------------------------------------------------------------------
-- | character ranges
--
digit, any, enter, anyButEnter :: Regexp () Action
digit = alt digit'
any   = alt any'
enter = alt enter'
anyButEnter = alt (any' \\ enter')

enter', any', digit' :: [Char]
enter'   = ['\n', '\r']
any'     = ['\0' .. '\255']
digit'   = ['0' .. '9']

-- ---------------------------------------------------------------------
-- | simple command mode keys (ones that don't consume input, or switch modes)
cmdc :: Regexp () Action
cmdc     = alt cmdc'

cmdc' :: [Char]
cmdc'    = cmdctrl' ++ cursc' ++ special' ++ upper' ++ lower'

special', upper', lower', cmdctrl', cursc' :: [Char]
special' = " !#$%()+,-.|0;<?@~"
upper'   = "BDEFGNHJLMPQRTUWXY"
lower'   = "hjklnpqx"
cmdctrl' = ['\^A','\^B','\^D','\^E','\^F','\^H','\^J','\^L','\^M','\^N',
            '\^P','\^R','\^T','\^U','\^W','\^Y','\^Z','\ESC','\^]','\^^']
cursc'   = [keyPPage, keyNPage, keyLeft, keyDown, keyUp]

