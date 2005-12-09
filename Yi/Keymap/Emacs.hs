--
-- Copyright 2005 by B.Zapf
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

 < stepcut> (1) showing the 'C-x - ' stuff in the message bar

check :)

 < stepcut> (2) allowing tab completion of M-x commands

still takes some time

2a) making bottom line dialogs possible
check
2b) sane cursor interaction
how?
2c) tab completion should be done via a tree-based algorithm

 < stepcut> (3) supporting 'C-h k'

it's C-x C-d at the moment, but - check

 < stepcut> (4) rebinding keys (of course)

no interface, but supported

 < stepcut> (5) C-u

interface is there, although a little crude


sun 31 jul

Great news, i'm including some "completion" algorithms this time,
these might help with the Filename and M-x interfaces.

Concerning M-x, there was some debate how M- is implemented, I think
the consus was that ESC prefix is the lowest common denominator of
many setups and thus should be realized first.

-}

--
-- | An emulation of the Emacs editor
--
-- This is a revised version (internal count 2) - the first version
-- lacked the possibility to abstract key bindings meaningfully.

{-

Currently there's three abstraction layers in here:

- Keypresses come in, parsed by a fast lazy lexer to ReflectableActions
- Those are translated to IOAction, depending on the RAHandler, which
  tries to make sense in special modes (like, the bottomline mode).
- to do this, it interprets EditorCommands which can be converted to
  Actions (IO ()) if necissary.

-}

module Yi.Keymap.Emacs where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )
import Yi.Lexers hiding (Action)

import Data.Char            ( chr, ord )
import Data.List
import qualified Data.Map as Map

import Yi.Keymap.Completion

type Key = Int {- What to use here - Meta Keystrokes? -}

type ActRef = String

-- A regex is bound to an Action (or rather, a reference to one)

data KeyBinding =
    KeyBinding [(Regexp EmacsState Action  ,   ActRef)]

type EmacsMode = Lexer EmacsState Action

type KeymapSetup = [KeyBinding]

type RAHandler = (ReflectableAction->(Meta EmacsState Action))

type Input = Maybe (Either String Int)
{- State consists of a Handler for ReflectableActions,
   then Maybe some String that's to be displayed in the bottom line,
   and a bunch of keymaps -}

-- TODO: this should be a record, i suppose (thanks xerox)

data EmacsState = ES RAHandler (Maybe String) KeymapSetup Input
    (CompletionTree Char)

getHandler :: EmacsState -> RAHandler
getHandler (ES x _ _ _ _) = x

getDisplay :: EmacsState -> (Maybe String)
getDisplay (ES _ x _ _ _) = x

getKeymaps :: EmacsState -> KeymapSetup
getKeymaps (ES _ _ x _ _) = x

getInput :: EmacsState -> Input
getInput (ES _ _ _ x _) = x

getCompletion :: EmacsState -> (CompletionTree Char)
getCompletion (ES _ _ _ _ x) = x


replaceHandler :: RAHandler -> EmacsState -> EmacsState
replaceHandler x (ES _ b c d e) = ES x b c d e

replaceDisplay :: Maybe String -> EmacsState -> EmacsState
replaceDisplay x (ES a _ c d e) = ES a x c d e

replaceKeymaps :: KeymapSetup -> EmacsState -> EmacsState
replaceKeymaps x (ES a b _ d e) = ES a b x d e

replaceInput :: Input -> EmacsState -> EmacsState
replaceInput x (ES a b c _ e) = ES a b c x e

replaceCompletion :: (CompletionTree Char) -> EmacsState -> EmacsState
replaceCompletion x (ES a b c d _) = ES a b c d x


-- ... and this was why it should be a record


-- Kinds of things that might happen to the editor.

data ReflectableAction =
    Action EditorCommand |
    Char (Char->ReflectableAction) |
    Input (Input->ReflectableAction) |
    CInput (Input->ReflectableAction) | -- with completion
    Keys (String->ReflectableAction) |
    Keymap Int |
    Cancel |
    Describe |
    Label String ReflectableAction |
    Accept | -- probably not needed
    Repeat Int

type Feedback = String



-- Keymap Name->Function association is done in a

type ActionMap = Map.Map String ReflectableAction

-- which is resolved when the keymap is being switched.

type MetaResult = (Maybe (Either Error Action),EmacsState,Maybe (Lexer EmacsState Action))


-- Feel free to supply any EditorCommands that are missing

data EditorCommand = ECinsertE String | ECdeleteE |
                     ECdownE | ECupE | ECrightE | ECleftE |
                     ECtopE | ECbotE | ECquitE | ECbkspE

translate :: EditorCommand -> IO ()
translate (ECinsertE x) = foldl' (>>) nopE $ map insertE x
translate ECdeleteE = leftE >> deleteE
translate ECdownE = downE
translate ECupE = upE
translate ECrightE = rightE
translate ECleftE = leftE
translate ECtopE = topE
translate ECbotE = botE
translate ECquitE = quitE
translate ECbkspE = leftE>>deleteE

repeat_fun :: Input->ReflectableAction
repeat_fun (Just (Right i)) = Repeat i
repeat_fun a                = error $ "Can't repeat "++(show a)++" times.";

-- actions  -   i dont know what to do with this big fat ugly list

actionlist :: ActionMap
actionlist = Map.fromList $ (map (\(Label a b)->(a,Label a b)))
    [Label "insert_self"  $ Keys $ \x  -> Action $ ECinsertE x,
     Label "cursor_up"    $ Keys $ \_  -> Action ECupE,
     Label "backspace"    $ Keys $ \_  -> Action ECbkspE,
     Label "delete"       $ Keys $ \_  -> Action ECdeleteE,
--     Label "kill"         $ Keys $ \_  -> Action ECkillE,
     Label "cursor_down"  $ Keys $ \_  -> Action ECdownE,
     Label "cursor_right" $ Keys $ \_  -> Action ECrightE,
     Label "cursor_left"  $ Keys $ \_  -> Action ECleftE,
     Label "top"          $ Keys $ \_  -> Action ECtopE,
     Label "bottom"       $ Keys $ \_  -> Action ECbotE,
     Label "mode_norm"    $ Keys $ \_  -> Keymap 0,
     Label "mode_cx"      $ Keys $ \_  -> Keymap 1,
     Label "describe_key" $ Keys $ \_  -> Describe,
     Label "quit"         $ Keys $ \_  -> Action ECquitE,
     Label "repeat_key"   $ Keys $ \_  -> Input repeat_fun,
     Label "accept"       $ Keys $ \_  -> Accept,
     Label "test_completion" $ Keys $ \_  -> CInput (\_->Keymap 0)
{-     Label "sol"          $ String $ \_  -> Action solE,
     Label "eol"          $ String $ \_  -> Action eolE,
     Label "upScreen"     $ String $ \_  -> Action upScreenE,
     Label "downScreen"   $ String $ \_  -> Action downScreenE,
     Label "refresh"      $ String $ \_  -> Action refreshE,
     Label "suspend"      $ String $ \_  -> Action suspendE,
     Label "split"        $ String $ \_  -> Action splitE,
     Label "close"        $ String $ \_  -> Action closeE,
     Label "nextWin"      $ String $ \_  -> Action nextWinE,
     Label "prevWin"      $ String $ \_  -> Action prevWinE,
     Label "fwrite"       $ String $ \_  -> Action fwriteE,
     Label "test_actions" $ String $ \_  -> Action cmdlineFocusE,
-} ]

-- A helper... Prepends an action

(>>>) :: Action -> MetaResult -> MetaResult
a >>> (Just (Right a'),b,c) = (Just $ Right $ a>>a',b,c)
a >>> (Nothing,b,c) = (Just $ Right $ a,b,c)
_ >>> b                     = b


-- Annotate characters below 32 with C-

printeable :: String->String
printeable (a:ta) | (ord a)<32 = "C-"++[chr ((ord a)+96)]++" "++(printeable ta)
                  | otherwise  = [a]++(printeable ta)
printeable [] = []

inputDisplay :: EmacsState -> EmacsState
inputDisplay x = replaceDisplay (Just $ newoutput) x
  where newoutput  = case getInput x of
                       Nothing -> ""
                       Just (Left s)  -> s
                       Just (Right i) -> show i


-- Switch keymap, display keys that caused the switch

switchkeymap :: Int->Meta EmacsState Action
switchkeymap i inp state =
    let ES h display keymaps userinput completion = state
        newdisplay = case display of
                       Just a  -> Just $ a++(printeable inp)
                       Nothing -> Just (printeable inp) in
        (Nothing,
         ES h newdisplay keymaps userinput completion,
         Just $ realizekeymap (keymaps !! i) )

{- This is what normally happens to ReflectableActions: Heading Labels are
   matched away, the String-RA that's beneath gets the user input thrown
   at. The user will always trigger an action with keys, so the Action
   always needs to handle the keypresses. -}

toplevel :: RAHandler
toplevel act inp state =
  case act of
    Label _ act' -> toplevel act' inp state -- skip labels
    Keys    act' -> toplevel (act' inp) inp state

    -- The following can only be a pended actions

    Keymap i   -> switchkeymap i inp state
    Repeat k   -> (Nothing,
                   replaceHandler (actrepeat k) state,
                   Nothing)
    Input act' -> (Just $ Right $ cmdlineFocusE,
                   replaceHandler (bottomlineinput act') $
                   replaceInput (Just (Right 0)) state,
                   Nothing)
    CInput act' -> (Just $ Right $ cmdlineFocusE,
                    replaceHandler (completioninput act') $
                    replaceInput (Just (Left "")) state,
                    Nothing)
    Action ec -> (Just $ Right $ translate ec,
                  replaceDisplay (Just "") state,
                  Just $ realizekeymap $ (getKeymaps state) !! 0)
    Describe   -> let keymaps = getKeymaps state in
                      (Nothing,
                       replaceHandler describe $
                       replaceDisplay (Just "Type Key to Describe: ") $
                       state,
                       Just $ realizekeymap $ keymaps !! 0)
    _ -> error "WTF Non toplevel action"


{-
  Bottom Line Input is brittle still - you need to finish the
  input with the return key. - If anyone has a great idea, I won't
  stop him.
-}

inputProcess :: String -> EmacsState -> EmacsState
inputProcess s (ES a b c d e) = ES a b c (inputProcess' d s) e

inputProcess' :: Input -> String -> Input
inputProcess' Nothing          b = Just $ Left b
inputProcess' (Just (Left a))  b = Just $ Left $ a++b
inputProcess' (Just (Right a)) b = Just $ Right $ (a*10)+(read b)

bottomlineinput :: (Input->ReflectableAction) -> RAHandler

{- first parameter is the action that needs a parameter -}

bottomlineinput pending act inp state =
  case act of
    Label _ act' -> bottomlineinput pending act' inp state -- skip labels
    Keys act' ->
      case act' inp of  -- feed the keypress
        Action ec -> case ec of
           ECinsertE c -> case c of -- this could be a regex match
                           '\r':_ -> cmdlineUnFocusE >>>
                               toplevel (pending $ getInput state) inp state
                           _    -> (Nothing,
                                    inputDisplay
                                    $ inputProcess c state,
                                    Nothing)
           _ -> error "Untreatable EditorCommand"
        Input f -> toplevel (f (getInput state)) inp state
        _ -> error "Untreatable action"
    _ -> error "Actions have to take a String as their first argument"

obvious' :: Eq a => (CompletionTree a) -> [a] -> [a]
obvious' c i = fst $ obvious $ snd $ complete c i

completeInput :: EmacsState -> EmacsState

completeInput s = replaceInput
                  (Just $ Left $ inp ++ obvious' (getCompletion s) inp)
                  s
   where inp = (\(Just (Left x))->x) (getInput s)

completioninput :: (Input->ReflectableAction) -> RAHandler

completioninput pending act inp state =
  case act of
    Label _ act' -> completioninput pending act' inp state -- skip labels
    Keys act' ->
      case act' inp of  -- feed the keypress
        Action ec -> case ec of
           ECinsertE c -> case c of
                           '\r':_ -> (Just $ Right cmdlineUnFocusE,
                                      replaceHandler toplevel state,
                                      Nothing)
                           '\^I':_ -> (Nothing,
                                       inputDisplay
                                       $ completeInput state,
                                       Nothing)
                           _    -> (Nothing,
                                    inputDisplay
                                    $ inputProcess c state,
                                    Nothing)
           _ -> error "Untreatable EditorCommand"
        Input f -> toplevel (f (getInput state)) inp state
        _ -> error "Untreatable action"
    _ -> error "Actions have to take a String as their first argument"



-- Repetition.

actrepeat :: Int -> RAHandler
actrepeat k act inp state =
  case act of
    Label _ act' -> actrepeat k act' inp state -- skip labels
    Keys act' ->
      case act' inp of
        Action act'' -> (Just $ Right $ foldr1 (>>) $ replicate k $ translate act'',
                         replaceHandler toplevel state,
                         Nothing)
        _            -> error "Untreatable Action"
    _ -> error "Actions have to take a String as their first argument"



-- Display the head label of a ReflectableAction

describe :: RAHandler
describe act inp state =
  case act of
    Label label act' ->
      case act' of
        Keys act'' -> case (act'' inp) of
          Keymap i   -> switchkeymap i inp state
          _          -> let keymaps=getKeymaps state in
                        (Nothing,
                         replaceHandler toplevel $
                         replaceDisplay (Just label) $
                         state,
                         Just $ realizekeymap (keymaps !! 0) )
        _  -> error "Describing an action that doesn't take a String as its first parameter"
    _             -> error "Unlabelled Action"


-- Some UI-Feedback Logic -- there should be an easier way to do that


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

                [(alt $ map chr $ 13:9:[32..127] , "insert_self"),
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
                 (char '\^T'   ,"test_completion"),
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
             (cs,ES toplevel Nothing [normalkeymap,cxkeymap] Nothing
                    (mergeTrees $ map listToTree $
                                map fst $ Map.toList actionlist))

