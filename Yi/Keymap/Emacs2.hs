{-# OPTIONS -fglasgow-exts #-}

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



module Yi.Keymap.Emacs2 ( keymap ) where

import Yi.Editor hiding     ( keymap )
import Yi.Yi hiding         ( keymap, meta )
--import Yi.Lexers hiding (Action)

import Yi.Char
import Data.Char           
--import Data.List
import qualified Yi.Map as M
import Data.Maybe
import Data.List
import Control.Monad.Writer
import Control.Monad.State
import Data.Dynamic
import Yi.Window
import Yi.Buffer

-- * Dynamic state components

newtype UniversalArg = UniversalArg (Maybe Int)
    deriving Typeable

instance Initializable UniversalArg where
    initial = return $ UniversalArg Nothing


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

-- * Killring

-- * Keymaps (rebindings)

-- doing the argument precisely is kind of tedious.
-- read: http://www.gnu.org/software/emacs/manual/html_node/Arguments.html
-- and: http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_318.html



-- | The command type. 

type KProc a = StateT String (Writer [Action]) a


showKey :: String->String
showKey ('\ESC':a:ta) = "M-" ++ [a] ++ showKey ta
showKey ('\ESC':ta) = "ESC " ++ showKey ta
showKey (a:ta) | ord a < 32 = "C-" ++ [chr (ord a + 96)] ++ " " ++ showKey ta
               | otherwise  = [a, ' '] ++ showKey ta
showKey [] = []


-- * The keymap abstract definition
c_ :: String -> String
c_ = map ctrlLowcase

m_ :: String -> String 
m_ s = concat [['\ESC', c] | c <- s] 

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
normalKlist = [ ([chr c], liftC $ insertSelf) | c <- [32..127] ] ++
              [
--       ((c_ " "),                liftC $ setMarkE),
         ((c_ "a"),                liftC $ repeatingArg solE),
         ((c_ "b"),                liftC $ repeatingArg leftE),
         ((c_ "d"),                liftC $ repeatingArg deleteE),
         ((c_ "e"),                liftC $ repeatingArg eolE),
         ((c_ "f"),                liftC $ repeatingArg rightE),
         ((c_ "g"),                liftC $ msgE "Quit"),
--       ((c_ "i"),                liftC $ indentC),
         ((c_ "j"),                liftC $ repeatingArg $ insertE '\n'),
--       ((c_ "k"),                liftC $ killLineC),
         ((c_ "m"),                liftC $ repeatingArg $ insertE '\n'),
         ((c_ "n"),                liftC $ repeatingArg downE),
         ((c_ "o"),                liftC $ repeatingArg (insertE '\n' >> leftE)),
         ((c_ "p"),                liftC $ repeatingArg upE),
         ((c_ "q"),                insertNextC),
--       ((c_ "r"),                liftC $ backwardsIncrementalSearchE),
--       ((c_ "s"),                liftC $ incrementalSearchE),
         ((c_ "t"),                liftC $ repeatingArg $ swapE),         
         ((c_ "u"),                readArgC),
--       ((c_ "v"),                liftC $ scrollDownC),                    
--       ((c_ "w"),                liftC $ killRegionC),                    
         ((c_ "x" ++ c_ "c"),      liftC $ quitE),
         ((c_ "x" ++ c_ "f"),      liftC $ findFile),
         ((c_ "x" ++ c_ "s"),      liftC $ fwriteE),
         ((c_ "x" ++ "o"),         liftC $ nextWinE),
         ((c_ "x" ++ "k"),         liftC $ closeE),
         ((c_ "x" ++ "r" ++ "k"),  liftC $ msgE "killRect"),
--       ((c_ "x" ++ "u"),         undoC), 
--       ((c_ "y"),                yankC),
         ((m_ "<"),                liftC $ repeatingArg topE),
         ((m_ ">"),                liftC $ repeatingArg botE),
--       ((m_ "%"),                searchReplaceC),
         ((m_ "c"),                liftC $ repeatingArg capitaliseWordE),
         ((m_ "d"),                liftC $ repeatingArg killWordE),
         ((m_ "f"),                liftC $ repeatingArg nextWordE),
         ((m_ "l"),                liftC $ repeatingArg lowercaseWordE),
         ((m_ "u"),                liftC $ repeatingArg uppercaseWordE),         
         ((m_ "w"),                liftC $ msgE "copy"),         
         ([keyLeft],               liftC $ repeatingArg leftE),
         ([keyRight],              liftC $ repeatingArg rightE),
         ([keyUp],                 liftC $ repeatingArg upE),
         ([keyDown],               liftC $ repeatingArg downE),
         (("\263"),                liftC $ repeatingArg bdeleteE),
         (("\BS"),                 liftC $ repeatingArg bdeleteE),
         ((m_ "\BS"),              liftC $ repeatingArg bkillWordE)
         
        ]

-- * Boilerplace code for the Command monad

liftC :: Action -> KProc ()
liftC = tell . return

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




withUnivArg :: (Int -> Action) -> Action
withUnivArg cmd = do UniversalArg s <- getDynamic
                     cmd (fromMaybe 1 s)
                     setDynamic $ UniversalArg Nothing

repeatingArg :: Action -> Action
repeatingArg f = withUnivArg $ \n->replicateM_ n f

insertSelf :: Action
insertSelf = repeatingArg $ do TypedKey k <- getDynamic
                               mapM_ insertE k

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
       setWindow w
       metaM (fromKProc $ makeKeymap klist)


rebind :: KList -> String -> KProc () -> KList
rebind kl k kp = M.toList $ M.insert k kp $ M.delete k $ M.fromList kl
         
                     
findFile :: Action
findFile = spawnMinibuffer "find file:" (rebind normalKlist (c_ "j") (liftC loadFile))

loadFile :: Action
loadFile = do MiniBuf w b <- getDynamic
              filename <- elemsB b -- doesn't seem to work
              deleteWindow $ Just w -- ???
              msgE $ "loading " ++ filename
              fnewE filename 
              return ()

-- * KeyList => keymap
-- Specialized version of MakeKeymap

data KME = KMESubmap KM
         | KMECommand (KProc ())

type KM = M.Map Char KME

type KListEnt = ([Char], KProc ())
type KList = [KListEnt]

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> KProc ()              
makeKeymap kmap = do getActions "" (buildKeymap M.empty kmap)
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
buildKeymap :: KM -> KList -> KM
buildKeymap fm_ l =
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
keymap = fromKProc $ makeKeymap normalKlist
