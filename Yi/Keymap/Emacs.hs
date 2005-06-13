
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

check :)

 < stepcut> (2) allowing tab completion of M-x commands

will take some time

 < stepcut> (3) supporting 'C-h k'

it's C-x C-d at the moment, but - check

 < stepcut> (4) rebinding keys (of course)

no interface, but supported

 < stepcut> (5) C-u

no interface, but supported

-}

--
-- | An emulation of the Emacs editor
--
-- This is a revised version (internal count 2) - the first version
-- lacked the possibility to abstract key bindings meaningfully.

module Yi.Keymap.Emacs where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )
import Yi.Lexers hiding (Action)

import Data.Char            ( chr, ord )
import Data.List
import qualified Data.Map as Map

type Key = Int {- What to use here - Meta Keystrokes? -}

type ActRef = String 

-- A regex is bound to an Action (or rather, a reference to one)

data KeyBinding = 
    KeyBinding [(Regexp EmacsState Action  ,   ActRef)]

type EmacsMode = Lexer EmacsState Action

type KeymapSetup = [KeyBinding]

type RAHandler = (ReflectableAction->(Meta EmacsState Action))

{- State consists of a Handler for ReflectableActions,
   then Maybe some String that's to be displayed in the bottom line,
   and a bunch of keymaps -}

-- TODO: this should be a record, i suppose (thanks xerox)

data EmacsState = ES RAHandler (Maybe String) KeymapSetup 

getHandler :: EmacsState -> RAHandler
getHandler (ES x _ _) = x

getDisplay :: EmacsState -> (Maybe String)
getDisplay (ES _ x _) = x

getKeymaps :: EmacsState -> KeymapSetup
getKeymaps (ES _ _ x) = x

replaceHandler :: RAHandler -> EmacsState -> EmacsState 
replaceHandler x (ES _ b c) = ES x b c

replaceDisplay :: Maybe String -> EmacsState -> EmacsState 
replaceDisplay x (ES a _ c) = ES a x c

replaceKeymaps :: KeymapSetup -> EmacsState -> EmacsState 
replaceKeymaps x (ES a b _) = ES a b x

-- ... and this was why it should be a record


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
    Accept | -- probably not needed
    Repeat Int

type Feedback = String


--type ActionMapEntry = (String,[String],ReflectableAction)
type ActionMap = Map.Map String ReflectableAction

-- actions  -   i dont know what to do with this big fat ugly list

actionlist :: ActionMap
actionlist = Map.fromList $ (map (\(Label a b)->(a,Label a b)))
    [Label "insert_self"  $ String $ \x  -> 
         Action (foldl' (>>) nopE (map insertE x)), 
     Label "cursor_up"    $ String $ \_  -> Action upE,
     Label "backspace"    $ String $ \_  -> Action (leftE>>deleteE),
     Label "delete"       $ String $ \_  -> Action deleteE,
     Label "kill"         $ String $ \_  -> Action killE,
     Label "cursor_down"  $ String $ \_  -> Action downE,
     Label "cursor_right" $ String $ \_  -> Action rightE,
     Label "cursor_left"  $ String $ \_  -> Action leftE,
     Label "top"          $ String $ \_  -> Action topE,
     Label "bottom"       $ String $ \_  -> Action botE,
     Label "sol"          $ String $ \_  -> Action solE,
     Label "eol"          $ String $ \_  -> Action eolE,
     Label "upScreen"     $ String $ \_  -> Action upScreenE,
     Label "downScreen"   $ String $ \_  -> Action downScreenE,
     Label "quit"         $ String $ \_  -> Action quitE,
     Label "accept"       $ String $ \_  -> Accept,
     Label "refresh"      $ String $ \_  -> Action refreshE,
     Label "suspend"      $ String $ \_  -> Action suspendE,
     Label "split"        $ String $ \_  -> Action splitE,
     Label "close"        $ String $ \_  -> Action closeE,
     Label "nextWin"      $ String $ \_  -> Action nextWinE,
     Label "prevWin"      $ String $ \_  -> Action prevWinE,
     Label "fwrite"       $ String $ \_  -> Action fwriteE,
     Label "mode_norm"    $ String $ \_  -> Keymap 0,
     Label "mode_cx"      $ String $ \_  -> Keymap 1,
     Label "describe_key" $ String $ \_  -> Describe,
     Label "test_actions" $ String $ \_  -> Action cmdlineFocusE,
     Label "repeat_key"   $ String $ \_  -> Int $ \k -> Repeat k ]

-- Annotate characters below 32 with C-

printeable :: String->String
printeable (a:ta) | (ord a)<32 = "C-"++[chr ((ord a)+96)]++" "++(printeable ta)
                  | otherwise  = [a]++(printeable ta)
printeable [] = []

-- Switch keymap, display keys that caused the switch

switchkeymap :: Int->Meta EmacsState Action
switchkeymap i inp state = 
    let ES h display keymaps = state 
        newdisplay = case display of
                       Just a  -> Just $ a++(printeable inp) 
                       Nothing -> Just (printeable inp) in
        (Nothing,
         ES h newdisplay keymaps,
         Just $ realizekeymap (keymaps !! i) )

{- This is what normally happens to ReflectableActions: Heading Labels are 
   matched away, the String-RA that's beneath gets the user input thrown 
   at.    -}

interpret :: RAHandler
interpret act inp state = 
  case act of 
    Label _ act' -> interpret act' inp state -- skip labels
    String act' -> 
      case act' inp of  -- feed the keypress
        Action act'' -> (Just $ Right act'',
                         replaceDisplay (Just "") state,
                         Just $ realizekeymap $ (getKeymaps state) !! 0)
        Keymap i   -> switchkeymap i inp state
        Describe   -> let keymaps = getKeymaps state in 
                      (Nothing,
                       replaceHandler describe $
                       replaceDisplay (Just "Type Key to Describe: ") $
                       state,
                       Just $ realizekeymap (keymaps !! 0))
        Repeat k   -> (Nothing,
                       replaceHandler (actrepeat k) state,
                       Nothing)
        _          -> error "Untreatable Action"
    _ -> error "Actions have to take a String as their first argument"


-- Repetition. This doesn't work yet since we can't input integers

actrepeat :: Int -> RAHandler
actrepeat k act inp state = 
  case act of 
    Label _ act' -> actrepeat k act' inp state -- skip labels
    String act' ->
      case act' inp of 
        Action act'' -> (Just $ Right (foldl (>>) nopE (replicate k act'')),
                         replaceHandler interpret state,
                         Nothing)
        _            -> error "Untreatable Action"
    _ -> error "Actions have to take a String as their first argument"



-- Display the head label of a ReflectableAction 

describe :: ReflectableAction -> Meta EmacsState Action
describe act inp state = 
  case act of 
    Label label act' -> 
      case act' of
        String act'' -> case (act'' inp) of
          Keymap i   -> switchkeymap i inp state 
          _          -> let keymaps=getKeymaps state in
                        (Nothing,
                         replaceHandler interpret $ 
                         replaceDisplay (Just label) $ 
                         state,
                         Just $ realizekeymap (keymaps !! 0) )
        _  -> error "Describing an action that doesn't take a String as its first parameter" 
    _             -> error "Unlabelled Action"


-- Some UI-Feedback Logic -- there should be an easier way to do that

type MetaResult = (Maybe (Either Error Action),EmacsState,Maybe (Lexer EmacsState Action))

showfeedback :: MetaResult -> MetaResult

showfeedback (Just (Right act),state,lexer) =
    case getDisplay state of
      Just msg -> (Just $ Right $ msgE msg >> act ,state,lexer)
      Nothing  -> (Just $ Right act,state,lexer)

showfeedback (Nothing         ,state,lexer) = 
    case getDisplay state of 
      Just msg -> (Just $ Right $ msgE msg        ,state,lexer)
      Nothing  -> (Nothing,state,lexer)

showfeedback junk = junk 


{- Finally we're building some lexers from regexes and action names 
   Take a Regex, Look up an action in the map, meta it to the regex
   and return the resulting lexer. -}

associateaction :: Regexp EmacsState Action -> String -> Lexer EmacsState Action
associateaction regex actionname   =
  regex `meta` \inp state ->
     let act= actionlist Map.! actionname  in       
           showfeedback $ (getHandler state) act inp state


-- Realize a whole key binding, basically a wrapper for associateaction

realizekeymap :: KeyBinding -> EmacsMode
realizekeymap (KeyBinding keymap_) = 
    foldr
    (>||<) -- A keymap is a lexer alternative
    ((char $ chr 0) `action` \_->Nothing) -- neutral lexer (stupid!)
    (map (\(x,y)-> associateaction x y) keymap_)


-- a few default bindings

normalkeymap :: KeyBinding
normalkeymap = KeyBinding

                [(alt $ map chr $ 13:[32..127] , "insert_self"),
                 (char keyUp   ,"cursor_up"),
                 (char keyDown ,"cursor_down"),
                 (char keyRight,"cursor_right"),
                 (char keyLeft ,"cursor_left"),
                 (char keyEnd  ,"eol"),
                 (char keyHome ,"sol"),
                 (char keyNPage,"downScreen"),
                 (char keyPPage,"upScreen"),
                 (char '\^U'   ,"repeat_key"),
                 (char '\^K'   ,"kill"),
                 (char keyBackspace  ,"backspace"),
                 (char '\^X'   ,"mode_cx")]

cxkeymap :: KeyBinding
cxkeymap = KeyBinding [((char '\^C'),"quit"),
                       ((char '\^D'),"describe_key"),
                       ((char '\^S'),"fwrite"),
                       (alt $ map chr $ [32..127],"mode_norm"),
                       ((char 't'),"test_actions")
                      ]

keymap :: [Char] -> [Action]
keymap cs = actions 
   where (actions,_,_) = 
             execLexer 
             (realizekeymap normalkeymap) 
             (cs,ES interpret Nothing [normalkeymap,cxkeymap])

