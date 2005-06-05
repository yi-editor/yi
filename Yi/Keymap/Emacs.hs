
-- 
-- Copyright 2005 by B.Zapf
--
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


{-

Requirements from the community:

sat 9 apr 22:20
 < stepcut> (1) showing the 'C-x - ' stuff in the message bar
 < stepcut> (2) allowing tab completion of M-x commands
 < stepcut> (3) supporting 'C-h k'
 < stepcut> (4) rebinding keys (of course)
 < stepcut> (5) C-u
 < basti_> doing WHAT? ;)
 * basti_ doesnt have that bound
 < stepcut> C-u is bound by default...
 < stepcut> try, C-u 20 a
 < basti_> ah
 < stepcut> it repeats a command
 < basti_> hmm ok
 < stepcut> C-u a will do it 4 times
 < stepcut> it is often, but not always interpreted as a repeat
 < stepcut> also, capturing/replaying keyboard macros
 < basti_> lets stay on the carpet.

 < stepcut> the hard part with (4) is how to express the command you want 
            to bind to at run-time
 < basti_> hmm.
 < basti_> is there some sort of eval function?
 < stepcut> in the compiled code, you just bind the key to some code that 
            is compiled
 < basti_> i know
 < basti_> we would need 2-way association
 < stepcut> right

-}


--
-- | An emulation of the Emacs editor
--
-- This is a revised version (internal count 2) - the first version
-- lacked the possibility to abstract key bindings meaningful.

module Yi.Keymap.Emacs where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )
import Yi.Lexers hiding (Action)

--import Data.Array
import Data.Char            ( chr, ord ) --, isAlphaNum )
import Data.List
import qualified Data.Map as Map
type Key = Int {- Meta Keystrokes? -}

type ActRef = String 

-- A regex is bound to an Action (or rather, a reference to one)

data KeyBinding = 
    KeyBinding [(Regexp EmacsState Action  ,   ActRef)]

type EmacsMode = Lexer EmacsState Action


{- State consists of visual Feedback, a stack of prompt strings,
   and Maybe something we're about to do. (The arguments will be 
   applied to ReflectableAction one by one, and when an "Action" 
   results, it is handed to the editor)
   -}

type Dialog = (Maybe String,Maybe ReflectableAction) 

type KeymapSetup = [KeyBinding]

data EmacsState = ES Dialog KeymapSetup

-- Kinds of things that might happen to the editor.

data ReflectableAction = 
    Action Action | 
    Char (Char->ReflectableAction) | 
    String (String->ReflectableAction) | 
    Int (Int->ReflectableAction) | 
    Keymap Int | 
    Cancel |
    Describe |
    Label String ReflectableAction |
    Accept |
    Repeat Int

type Feedback = String

-- ActionMap stores names, 

type ActionMapEntry = (String,[String],ReflectableAction)

type ActionMap = Map.Map String ReflectableAction

actionlist :: ActionMap
actionlist = Map.fromList $ (map (\(Label a b)->(a,Label a b)))
    [Label "insert_self"  $ String $ \[x]-> Action (insertE x),
     Label "cursor_up"    $ String $ \_  -> Action upE,
     Label "cursor_down"  $ String $ \_  -> Action downE,
     Label "cursor_right" $ String $ \_  -> Action rightE,
     Label "cursor_left"  $ String $ \_  -> Action leftE,
     Label "quit"         $ String $ \_  -> Action quitE,
     Label "accept"       $ String $ \_  -> Accept,
     Label "mode_norm"    $ String $ \_  -> Keymap 0,
     Label "mode_cx"      $ String $ \_  -> Keymap 1,
     Label "describe_key" $ String $ \_  -> Describe,
     Label "test_actions" $ String $ \_  -> Action cmdlineFocusE,
     Label "repeat_key"   $ String $ \_  -> Int $ \k -> Repeat k ]


printeable :: String->String
printeable (a:ta) | (ord a)<32 = "C-"++[chr ((ord a)+96)]++" "++(printeable ta)
                  | otherwise  = [a]++(printeable ta)
printeable [] = []

{- This is what normally happens to ReflectableActions: Heading Labels are 
   matched away, the String-RA below (hopefully) gets the user input thrown 
   at.    -}

interpret :: ReflectableAction -> Meta EmacsState Action
interpret act inp state = 
  let ES (feedback,pendaction) keymaps = state in
  case act of 
    Label _ act' -> interpret act' inp state -- skip labels
    String act' ->
      case act' inp of 
        Action act'' -> (Just $ Right act'',
                         ES (Just "",pendaction) keymaps,
                         Nothing)
        Keymap i   -> let newfb = case feedback of
                                    Just a  -> Just $ a++(printeable inp) 
                                    Nothing -> Just (printeable inp) in
                      (Nothing,
                       ES (newfb,pendaction) keymaps,
                       Just $ realizekeymap (keymaps !! i) )
        Describe   -> (Nothing,
                       ES (Just "Type Key to Describe: ",Just Describe) keymaps,
                       Just $ realizekeymap (keymaps !! 0))
        Repeat k   -> (Nothing,
                       ES (Nothing,Just $ Repeat k) keymaps,
                       Nothing)
        _          -> error "Untreatable Action"
    _ -> error "Actions have to take a String as their first argument"

type MetaResult = (Maybe (Either Error Action),EmacsState,Maybe (Lexer EmacsState Action))
--type MetaMetaResult = (Maybe (Either Error ReflectabeAction),EmacsState,Maybe (Lexer EmacsState Action))

{- Display the head label -}

describe :: ReflectableAction -> Meta EmacsState Action
describe act inp (ES (feedback,pendaction) keymaps) = 
  case act of 
    Label label act' -> 
      case act' of
        String act'' -> case (act'' inp) of
          Keymap i -> let newfb = case feedback of
                                  Just a  -> Just $ a++(printeable inp) 
                                  Nothing -> Just (printeable inp) in
                      (Nothing,
                       ES (newfb,pendaction) keymaps,
                       Just $ realizekeymap (keymaps !! i) )
          _         -> (Nothing,
                        ES (Just label,Nothing) keymaps,
                        Just $ realizekeymap (keymaps !! 0) )
        _  -> error "Describing an action that doesn't take a String as its first parameter" -- self-reference rocks!
    _             -> error "Unlabelled Action"


--inputint :: ReflectableAction -> Meta EmacsState Action
--inputint act inp (ES (feedback,pendaction) keymaps) =


{- Some UI-Feedback Logic -}

showfeedback :: MetaResult -> MetaResult
showfeedback (act,ES (Nothing,sc) sd,lexer) = 
    (act, ES (Nothing,sc) sd,lexer)
showfeedback (Just (Right act),ES (Just a,sc) sd,lexer) = 
    (Just $ Right $ msgE a >> act, ES (Just a,sc) sd,lexer)
showfeedback (Nothing,ES (Just a,sc) sd,lexer) = 
    (Just $ Right $ msgE a, ES (Just a,sc) sd,lexer)
showfeedback (junk,ES (sa,sb) sc,lexer) = 
    (junk, ES (sa,sb) sc,lexer)


{- Finally we're building some lexers from regexes and action names -}

associateaction :: Regexp EmacsState Action -> String -> Lexer EmacsState Action
associateaction regex actionname   =
  regex `meta` \inp state ->
     let act= actionlist Map.! actionname 
         ES (_,pendaction) _ = state in
           showfeedback $ case pendaction of 
             Nothing       -> interpret act inp state 
             Just Describe -> describe  act inp state 
--             Int  act'     -> inputint inp state
             _ -> error "Can't pend this action yet"

comparefsts :: (Ord a) => (a,b)->(a,b)->Ordering
comparefsts (x,_) (y,_) = compare x y

allkeys :: [Key]
allkeys = [1..255] -- is 0 a keystroke?


realizekeymap :: KeyBinding -> EmacsMode
realizekeymap (KeyBinding km) = 
    foldr

-- A keymap is a lexer alternative

    (>||<) 

-- neutral lexer (stupid!)

    ((char $ chr 0) `action` \_->Nothing)

    (map (\(x,y)-> associateaction x y) km) --Build lexers

normalkeymap :: KeyBinding
normalkeymap = KeyBinding

                [(alt $ map chr $ 13:[32..127] , "insert_self"),
                 (char keyUp   ,"cursor_up"),
                 (char keyDown ,"cursor_down"),
                 (char keyRight,"cursor_right"),
                 (char keyLeft ,"cursor_left"),
                 ((char '\^X') ,"mode_cx")]



cxkeymap :: KeyBinding
cxkeymap = KeyBinding [((char '\^C'),"quit"),
                       ((char '\^D'),"describe_key"),
                       (alt $ map chr $ [32..127],"mode_norm"),
                       ((char 't'),"test_actions")
                      ]


emacsNormal :: EmacsMode
emacsNormal = realizekeymap normalkeymap

keymap :: [Char] -> [Action]
keymap cs = actions 
   where (actions,_,_) = execLexer emacsNormal (cs,ES (Nothing,Nothing) [normalkeymap,cxkeymap])

