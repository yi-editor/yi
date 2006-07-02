--
-- Copyright (c) 2005 Jean-Philippe Bernardy
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
  This is my attempt at a new key bindings, the idea is to go somewhere
  in between of Vim and Emacs. So for example I want to keep a lot of
  the quick fire emacs commands such as C-x C-s to quickly save a file
  but then wouldn't mind a command mode for some of the less common
  commands.
  
  To begin with the mode is basically the same as the Emacs2 mode but
  with the Vim style '~' character for representing the empty lines
  after the end of the file displayed in the remainder of the window.
-}



module Yi.Keymap.Vimacs ( keymap ) where

import Yi.Editor hiding     ( keymap )
import Yi.Yi hiding         ( keymap, meta, string )
import Yi.Window
import Yi.Buffer
import qualified Yi.Style as Style

import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Keys

import Yi.Keymap.Movements ( moveRightWord
			   , movementCommand
			   , newlineAndIndent
			   , writeFileandMessage
			   )

import Data.Char
import Data.Maybe
import Data.List
import Data.Dynamic
import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad.State

-- * Dynamic state-components


newtype TypedKey = TypedKey String
    deriving Typeable

instance Initializable TypedKey where
    initial = return $ TypedKey ""

data MiniBuf = forall a. Buffer a => MiniBuf Window a
    deriving Typeable

instance Initializable MiniBuf where
    initial = do b <- stringToNewBuffer "*minibuf*" []
                 w <- newWindow b
                 return $ MiniBuf w b


-- TODO

-- * Keymaps (rebindings)

-- | The command type.

type KProc a = StateT String (Writer [Action]) a


-- * The keymap abstract definition


-- In the future the following list can become something like
-- [ ("C-x k", killBuffer) , ... ]
-- This structure should be easy to modify dynamically (for rebinding keys)

-- Ultimately, this should become:

--  [ ("C-x k", "killBuffer")
-- killBuffer would be looked up a la ghci. Then its type would be checked
-- (Optional) arguments could then be handled dynamically depending on the type
-- Action; Int -> Action; Region -> Action; types could be handled in a clever
-- way, reducing glue code to the minimum.

-- And, rebinding could then be achieved :)

normalKlist :: KList
normalKlist = [ ([c], atomic $ insertSelf) | c <- printableChars ] ++
              [
	 {-
	 Note that [-C-f-] and friends which are suppose to be
	 basically synonyms for left, right, up and down, won't
	 actually work for now, because I've mapped them to
	 [-C-<right>-] and equivalently for the other directions.
	 The reason for this is explained below, basically I can't
	 seem to represent [-C-<right>-].
	 @todo{This whole list would probably be better formatted more to
	 the left, just to give each action specification more room.}
	 -}
        ("DEL",       atomic $ repeatingArg deleteE),
        ("BACKSP",    atomic $ repeatingArg bdeleteE),
        ("C-M-w",     atomic $ appendNextKillE),
        ("C-_",       atomic $ undoE),
        ("C-SPC",     atomic $ (getPointE >>= setMarkE)),
	("C-g",       atomic $ unsetMarkE), 
         -- C-g should be a more general quit that also unsets the mark.
	 --      ("C-g",       atomic $ keyboardQuitE),

        ("C-a",       atomic $ repeatingArg solE),
        ("C-b",       atomic $ repeatingArg leftE),
        ("C-d",       atomic $ repeatingArg deleteE),
        ("C-e",       atomic $ repeatingArg eolE),
        ("C-f",       atomic $ repeatingArg rightE),

--      ("C-i",       atomic $ indentC),
        ("C-j",       atomic $ repeatingArg $ newlineAndIndent),
        ("C-k",       atomic $ killLineE),
        ("C-m",       atomic $ repeatingArg $ insertE '\n'),
        ("C-n",       atomic $ repeatingArg downE),
        ("C-o",       atomic $ repeatingArg (insertE '\n' >> leftE)),
        ("C-p",       atomic $ repeatingArg upE),
        ("C-q",       insertNextC),
--      ("C-r",       atomic $ backwardsIncrementalSearchE),
--      ("C-s",       atomic $ incrementalSearchE),
        ("C-t",       atomic $ repeatingArg $ swapE),
        ("C-u",       readArgC),
        ("C-v",       atomic $ scrollDownE),
        ("C-w",       atomic $ killRegionE >> unsetMarkE),
        ("C-y",       atomic $ yankE),
        ("C-z",       atomic $ suspendE),

	("C-x s",     atomic $ fwriteAllE),

        {- Those prefixed with C-x -}
        ("C-x ^",     atomic $ repeatingArg enlargeWinE),
        ("C-x 1",     atomic $ closeOtherE),
        ("C-x 2",     atomic $ splitE),
        ("C-x C-c",   atomic $ closeE),
        ("C-x C-f",   atomic $ findFile),
	("C-x C-g",   atomic $ findGotoLine), -- Alt-Shift-G some prefer?
        ("C-x C-s",   atomic $ writeFileandMessage),

        {- 
	   @bug{small bug somehow in the selection if the mark isn't set
           when this is applied (try it and see).}
	-}
        ("C-x C-x",   atomic $ exchangePointAndMarkE),
        ("C-x o",     atomic $ nextWinE),
        ("C-x k",     atomic $ closeE),
--      ("C-x r k",   atomic $ killRectE),
--      ("C-x r o",   atomic $ openRectE),
--      ("C-x r t",   atomic $ stringRectE),
--      ("C-x r y",   atomic $ yankRectE),
        ("C-x u",     atomic $ repeatingArg undoE),


        ("M-<",       atomic $ repeatingArg topE),
        ("M->",       atomic $ repeatingArg botE),
--      ("M-%",       searchReplaceC),
        ("M-BACKSP",  atomic $ repeatingArg bkillWordE),
        ("M-DEL",     atomic $ repeatingArg killWordE),
--      ("M-a",       atomic $ repeatingArg backwardSentenceE),
        ("M-b",       atomic $ repeatingArg prevWordE),
        ("M-c",       atomic $ repeatingArg capitaliseWordE),
        ("M-d",       atomic $ repeatingArg killWordE),
--      ("M-e",       atomic $ repeatingArg forwardSentenceE),
        ("M-f",       atomic $ repeatingArg nextWordE),
--      ("M-h",       atomic $ repeatingArg markParagraphE),
--      ("M-k",       atomic $ repeatingArg killSentenceE),
        ("M-l",       atomic $ repeatingArg lowercaseWordE),
--      ("M-t",       atomic $ repeatingArg transposeWordsE),
        ("M-u",       atomic $ repeatingArg uppercaseWordE),
        ("M-w",       atomic $ killRingSaveE)
--      , ("M-x",       atomic $ executeExtendedCommandE)
        , ("M-y",       atomic $ yankPopE)
        , ("<left>",    atomic $ repeatingArg $ movementCommand leftE       )
        , ("<right>",   atomic $ repeatingArg $ movementCommand rightE      )
        , ("<up>",      atomic $ repeatingArg $ movementCommand upE         )
        , ("<down>",    atomic $ repeatingArg $ movementCommand downE       )
        , ("<next>",    atomic $ repeatingArg $ movementCommand downScreenE )
        , ("<prior>",   atomic $ repeatingArg $ movementCommand upScreenE   )

        {-
	  Okay I cannot see how to represent control done with
	  a non-lowercase letter. [-C-SPC-] is done in [-Emacs/Keys.hs-]
	  but for everything else if you attempt to right for
	  example [-C-<right>-] it will basically just get mapped to
	  [-right-] since the parser in the [-Emacs/Keys.hs-] will call
	  [-ctrlLowcase-] in [-Yi/Char.hs-] which for all non-lowercase
	  letters just returns the input character.
	  Hence for now I have here some not quite perfect key mappings
	  which once I figure out how to represent the ones I really would
	  like, I will do. For each one the comment above shows the key
	  mapping that I would actually like.
        -}
        --"C-TAB"
	, ("C-x b",     atomic $ nextBufW)
	
	-- "C-<right>"
	, ("C-f", atomic $ repeatingArg moveRightWord)
        ]

-- * Boilerplate code for the Command monad

liftC :: Action -> KProc ()
liftC = tell . return

-- | Define an atomic interactive command.
-- Purose is to define "transactional" boundaries for killring, undo, etc.
atomic :: Action -> KProc ()
atomic cmd = liftC $ do cmd
                        killringEndCmd

getInput :: KProc String
getInput = get

putInput :: String -> KProc ()
putInput = modify . const


readStroke, lookStroke :: KProc Char
readStroke = do (c:cs) <- getInput
                putInput cs
                return c

lookStroke = do (c:_) <- getInput
                return c


-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents


insertSelf :: Action
insertSelf = repeatingArg $ do TypedKey k <- getDynamic
                               insertNE k

insertNextC :: KProc ()
insertNextC = do c <- readStroke
                 liftC $ repeatingArg $ insertE c



-- | Complain about undefined key
undefC :: Action
undefC = do TypedKey k <- getDynamic
            errorE $ "Key sequence not defined : " ++
                  showKey k ++ " " ++ show (map ord k)


-- | C-u stuff
readArgC :: KProc ()
readArgC = do readArg' Nothing

readArg' :: Maybe Int -> KProc ()
readArg' acc = do
    c <- lookStroke
    if isDigit c
     then (do { readStroke
              ; let acc' = Just $ 10 * (fromMaybe 0 acc) + (ord c - ord '0')
              ; liftC $ do TypedKey k <- getDynamic
                           msgE (showKey k ++ show (fromJust $ acc'))
              ; readArg' acc'
             }
          )
     else liftC $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc


-- TODO:
-- buffer local keymap: this requires Core support
-- ensure that it quits (ok[ret]/cancel[C-g])
-- add prompt
-- resize: this requires Core support
-- prevent recursive minibuffer usage
-- hide modeline

spawnMinibuffer :: String -> KList -> Action
spawnMinibuffer _prompt klist =
    do MiniBuf w _b <- getDynamic
       setWinE w
       metaM (fromKProc $ makeKeymap klist)

rebind :: KList -> String -> KProc () -> KList
rebind kl k kp = M.toList $ M.insert k kp $ M.fromList kl

findFile :: Action
findFile = spawnMinibuffer "find file:" (rebind normalKlist "C-j" (liftC loadFile))

-- read contents of current buffer (which should be the minibuffer), and
-- use it to open a new file
loadFile :: Action
loadFile = do filename <- liftM init readAllE  -- problems if more than 1 line, of course
              closeE
              msgE $ "loading " ++ filename
              fnewE filename

{-
  Do roughly the same for [-gotoLine-] which we do for [-findFile-] above.
  Obviously problems if the contents has more than one line or even if it
  just isn't a number.
-}
-- | Goto a line specified in the mini buffer.
findGotoLine :: Action
findGotoLine = spawnMinibuffer "goto line:" 
	       (rebind normalKlist "C-j" (liftC gotoLine))
	       where gotoLine :: Action
		     gotoLine = do lineString <- liftM init readAllE
				   let lineNo = read lineString
				   closeE
				   gotoLnE lineNo



scrollDownE :: Action
scrollDownE = withUnivArg $ \a ->
              case a of
                 Nothing -> downScreenE
                 Just n -> replicateM_ n downE




-- * KeyList => keymap
-- Specialized version of MakeKeymap

data KME = KMESubmap KM
         | KMECommand (KProc ())

type KM = M.Map Char KME

type KListEnt = ([Char], KProc ())
type KList = [KListEnt]

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> KProc ()
makeKeymap kmap = do getActions "" (buildKeymap kmap)
                     makeKeymap kmap

getActions :: String -> KM -> KProc ()
getActions k fm = do
    c <- readStroke
    let k' = k ++ [c]
    liftC $ setDynamic $ TypedKey k'
    case fromMaybe (KMECommand $ liftC undefC) (M.lookup c fm) of
        KMECommand m -> do liftC $ msgE ""
                           m
        KMESubmap sfm -> do liftC $ msgE (showKey k' ++ "-")
                            getActions k' sfm


-- | Builds a keymap (Yi.Map.Map) from a key binding list, also creating
-- submaps from key sequences.
buildKeymap :: KList -> KM
buildKeymap l = buildKeymap' M.empty [(readKey k, c) | (k,c) <- l]

buildKeymap' :: KM -> KList -> KM
buildKeymap' fm_ l =
    foldl addKey fm_ [(k, KMECommand c) | (k,c) <- l]
    where
        addKey fm (c:[], a) = M.insert c a fm
        addKey fm (c:cs, a) =
            flip (M.insert c) fm $ KMESubmap $
                case M.lookup c fm of
                    Nothing             -> addKey M.empty (cs, a)
                    Just (KMESubmap sm) -> addKey sm (cs, a)
                    _                   -> error "Invalid keymap table"
        addKey _ ([], _) = error "Invalid keymap table"


fromKProc :: KProc a -> [Char] -> [Action]
fromKProc kp cs = snd $ runWriter $ runStateT kp cs

-- | entry point
keymap :: [Char] -> [Action]
keymap cs = setWindowFillE '~' : winStyleAct : actions
	    where actions     = keymapFun cs
		  keymapFun   = fromKProc $ makeKeymap normalKlist
		  winStyleAct = setWindowStyleE defaultVimacsUiStyle

{-
  For now we just make the selected style the same as the eof characters
  style.
-}
defaultVimacsUiStyle :: Style.UIStyle
defaultVimacsUiStyle = Style.ui { selected = Style.eof Style.ui }