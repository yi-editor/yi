--
-- Copyright (c) 2005,2007 Jean-Philippe Bernardy
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



module Yi.Keymap.Emacs ( keymap ) where

import Yi.Editor hiding     ( keymap )
import Yi.Yi hiding         ( keymap, meta, string )
import Yi.Window
import Yi.Buffer

import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Keys

import Data.Char
import Data.Maybe
import Data.List
import Data.Dynamic
import qualified Data.Map as M
import qualified Yi.UI as UI  -- FIXME this module should not depend on UI

import Control.Monad.Writer
import Control.Monad.State

-- * Dynamic state-components

newtype TypedKey = TypedKey [Event]
    deriving Typeable

instance Initializable TypedKey where
    initial = return $ TypedKey []

data MiniBuf = MiniBuf Window FBuffer
    deriving Typeable

instance Initializable MiniBuf where
    initial = do b <- stringToNewBuffer "*minibuf*" []
                 w <- UI.newWindow b
                 return $ MiniBuf w b


-- TODO

-- * Keymaps (rebindings)

-- | The command type.

type KProc a = StateT [Event] (Writer [Action]) a


-- * The keymap abstract definition

normalKlist :: KList String
normalKlist = [ ([c], atomic $ insertSelf) | c <- printableChars ] ++
              [
        ("RET",      atomic $ repeatingArg $ insertE '\n'),
        ("DEL",      atomic $ repeatingArg deleteE),
        ("BACKSP",   atomic $ repeatingArg bdeleteE),
        ("C-M-w",    atomic $ appendNextKillE),
        ("C-/",      atomic $ repeatingArg undoE),
        ("C-_",      atomic $ repeatingArg undoE),
        ("C-<left>", atomic $ repeatingArg prevWordE),
        ("C-<right>",atomic $ repeatingArg nextWordE),
        ("C-SPC",    atomic $ (getPointE >>= setMarkE)),
        ("C-a",      atomic $ repeatingArg solE),
        ("C-b",      atomic $ repeatingArg leftE),
        ("C-d",      atomic $ repeatingArg deleteE),
        ("C-e",      atomic $ repeatingArg eolE),
        ("C-f",      atomic $ repeatingArg rightE),
        ("C-g",      atomic $ unsetMarkE), 
--      ("C-g",      atomic $ keyboardQuitE), -- C-g should be a more general quit that also unsets the mark.
--      ("C-i",      atomic $ indentC),
        ("C-j",      atomic $ repeatingArg $ insertE '\n'),
        ("C-k",      atomic $ killLineE),
        ("C-m",      atomic $ repeatingArg $ insertE '\n'),
        ("C-n",      atomic $ repeatingArg downE),
        ("C-o",      atomic $ repeatingArg (insertE '\n' >> leftE)),
        ("C-p",      atomic $ repeatingArg upE),
        ("C-q",               insertNextC),
--      ("C-r",      atomic $ backwardsIncrementalSearchE),
--      ("C-s",      atomic $ incrementalSearchE),
        ("C-t",      atomic $ repeatingArg $ swapE),
        ("C-u",               readArgC),
        ("C-v",      atomic $ scrollDownE),
        ("C-w",      atomic $ killRegionE),
        ("C-z",      atomic $ suspendE),
        ("C-x ^",    atomic $ repeatingArg enlargeWinE),
        ("C-x 1",    atomic $ closeOtherE),
        ("C-x 2",    atomic $ splitE),
        ("C-x C-c",  atomic $ quitE),
        ("C-x C-f",  atomic $ findFile),
        ("C-x C-s",  atomic $ fwriteE),
        ("C-x C-x",  atomic $ exchangePointAndMarkE),
        ("C-x o",    atomic $ nextWinE),
        ("C-x k",    atomic $ closeE),
--      ("C-x r k",  atomic $ killRectE),
--      ("C-x r o",  atomic $ openRectE),
--      ("C-x r t",  atomic $ stringRectE),
--      ("C-x r y",  atomic $ yankRectE),
        ("C-x u",    atomic $ repeatingArg undoE),
        ("C-x v",    atomic $ repeatingArg shrinkWinE),
        ("C-y",      atomic $ yankE),
        ("M-<",      atomic $ repeatingArg topE),
        ("M->",      atomic $ repeatingArg botE),
--      ("M-%",               searchReplaceC),
        ("M-BACKSP", atomic $ repeatingArg bkillWordE),
--      ("M-a",      atomic $ repeatingArg backwardSentenceE),
        ("M-b",      atomic $ repeatingArg prevWordE),
        ("M-c",      atomic $ repeatingArg capitaliseWordE),
        ("M-d",      atomic $ repeatingArg killWordE),
--      ("M-e",      atomic $ repeatingArg forwardSentenceE),
        ("M-f",      atomic $ repeatingArg nextWordE),
--      ("M-h",      atomic $ repeatingArg markParagraphE),
--      ("M-k",      atomic $ repeatingArg killSentenceE),
        ("M-l",      atomic $ repeatingArg lowercaseWordE),
--      ("M-t",      atomic $ repeatingArg transposeWordsE),
        ("M-u",      atomic $ repeatingArg uppercaseWordE),
        ("M-w",      atomic $ killRingSaveE),
--      ("M-x",      atomic $ executeExtendedCommandE),
        ("M-x g o t o - l i n e", atomic $ gotoLine), -- joke.
        ("M-y",      atomic $ yankPopE),
        ("<home>",   atomic $ repeatingArg solE),
        ("<end>",    atomic $ repeatingArg eolE),
        ("<left>",   atomic $ repeatingArg leftE),
        ("<right>",  atomic $ repeatingArg rightE),
        ("<up>",     atomic $ repeatingArg upE),
        ("<down>",   atomic $ repeatingArg downE),
        ("<next>",   atomic $ repeatingArg downScreenE),
        ("<prior>",  atomic $ repeatingArg upScreenE)
        ]

-- * Boilerplate code for the Command monad

liftC :: Action -> KProc ()
liftC = tell . return

-- | Define an atomic interactive command.
-- Purose is to define "transactional" boundaries for killring, undo, etc.
atomic :: Action -> KProc ()
atomic cmd = liftC $ do cmd
                        killringEndCmd

getInput :: KProc [Event]
getInput = get

putInput :: [Event] -> KProc ()
putInput = modify . const


readStroke, lookStroke :: KProc Event
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
                               insertNE (map eventToChar k)

insertNextC :: KProc ()
insertNextC = do c <- readStroke
                 liftC $ repeatingArg $ insertE (eventToChar c)



-- | Complain about undefined key
undefC :: Action
undefC = do TypedKey k <- getDynamic
            errorE $ "Key sequence not defined : " ++ showKey k ++ " " ++ show k


-- | C-u stuff
readArgC :: KProc ()
readArgC = do readArg' Nothing

readArg' :: Maybe Int -> KProc ()
readArg' acc = do
    c <- lookStroke
    case c of
      Event (KASCII d) [] | isDigit d ->
           do { readStroke
              ; let acc' = Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
              ; liftC $ do TypedKey k <- getDynamic
                           msgE (showKey k ++ show (fromJust $ acc'))
              ; readArg' acc'
             }
      _ -> liftC $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc


-- TODO:
-- buffer local keymap: this requires Core support
-- ensure that it quits (ok[ret]/cancel[C-g])
-- add prompt
-- resize: this requires Core support
-- prevent recursive minibuffer usage
-- hide modeline

spawnMinibuffer :: String -> KList String -> Action
spawnMinibuffer _prompt klist =
    do MiniBuf w _b <- getDynamic
       setWinE w
       metaM (fromKProc $ makeKeymap klist)

rebind :: KList String -> String -> KProc () -> KList String
rebind kl k kp = M.toList $ M.insert k kp $ M.fromList kl

findFile :: Action
findFile = withMinibuffer "find file:" $ \filename -> do msgE $ "loading " ++ filename
                                                         fnewE filename
-- | Goto a line specified in the mini buffer.
gotoLine :: Action
gotoLine = withMinibuffer "goto line:" $ \lineString -> gotoLnE (read lineString)

withMinibuffer :: String -> (String -> Action) -> Action
withMinibuffer prompt act = spawnMinibuffer prompt (rebind normalKlist "RET" (liftC innerAction))
    -- read contents of current buffer (which should be the minibuffer), and
    -- apply it to the desired action
    where innerAction :: Action
          innerAction = do lineString <- readAllE
                           closeE
                           act lineString

scrollDownE :: Action
scrollDownE = withUnivArg $ \a ->
              case a of
                 Nothing -> downScreenE
                 Just n -> replicateM_ n downE

-- * KeyList => keymap
-- Specialized version of MakeKeymap

data KME = KMESubmap KM
         | KMECommand (KProc ())

type KM = M.Map Event KME

type KListEnt k = (k, KProc ())
type KList k = [KListEnt k]

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList String -> KProc ()
makeKeymap kmap = do getActions [] (buildKeymap kmap)
                     makeKeymap kmap

getActions :: [Event] -> KM -> KProc ()
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
buildKeymap :: KList String -> KM
buildKeymap l = buildKeymap' M.empty [(readKey k, c) | (k,c) <- l]

buildKeymap' :: KM -> KList [Event] -> KM
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


fromKProc :: KProc a -> Keymap
fromKProc kp cs = snd $ runWriter $ runStateT kp cs

-- | entry point
keymap :: Keymap
keymap = fromKProc (makeKeymap normalKlist)


