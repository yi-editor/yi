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

module Yi.Keymap.Emacs2 (keymap) where


import Yi.Editor            ( Action )
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


-- | The state of the editor.
-- Should include much more things
-- (killring, current keylist, etc.)

data ES = ES { 
               esKillRing :: String, -- should be [String]
               esLatestIsKill :: Bool,
               esKeyList :: KList,
               esKeyMap :: KM, -- invariant: esKeyMap == buildKeymap esKeyList
               esKey :: String, 
               esArg :: Maybe Int 
               -- doing the argument precisely is kind of tedious.
               -- read: http://www.gnu.org/software/emacs/manual/html_node/Arguments.html
               -- and: http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_318.html
             }

-- NOTES:

-- 1. Maybe most of (all) the above stuff should not be part of the emacs state at all
-- but the editor state.

-- 2. The above requires a configurable editor state. Such a feature would allow
-- to clearly separate configuration of keybindings and configuration of behaviours.


-- | The command type. 

type KProc a = StateT (String, ES) (Writer [Action]) a
type Command = KProc ()


-- * Functions to handle the state.
initialState :: ES
initialState = ES { 
                   esKillRing = [],
                   esLatestIsKill = False,
                   esKeyList = normalKlist,
                   esKeyMap = buildKeymap M.empty (esKeyList initialState),
                   esKey = "", 
                   esArg = Nothing
                  }

esAddKey :: Char -> ES -> ES
esAddKey k s = s { esKey = esKey s ++ [k] }


showKey :: String->String
showKey ('\ESC':a:ta) = "M-" ++ [a] ++ showKey ta
showKey ('\ESC':ta) = "ESC " ++ showKey ta
showKey (a:ta) | ord a < 32 = "C-" ++ [chr (ord a + 96)] ++ " " ++ showKey ta
               | otherwise  = [a, ' '] ++ showKey ta
showKey [] = []


-- * The keymap abstract definition
ctrl :: String -> String
ctrl = map ctrlLowcase

meta :: String -> String 
meta s = concat [['\ESC', c] | c <- s] 

-- In the future the following list can become something like
-- [ ("C-x k", killBuffer) , ... ]
-- This structure should be easy to modify dynamically (for rebinding keys)

normalKlist :: KList 
normalKlist = [ ([chr c], insertSelfC) | c <- [32..127] ] ++
              [
--       ((ctrl " "), setMarkC),
         ((ctrl "a"), repeatingArgC $ liftC solE),
         ((ctrl "b"), repeatingArgC $ liftC leftE),
         ((ctrl "d"), repeatingArgC $ liftC deleteE),
         ((ctrl "e"), repeatingArgC $ liftC eolE),
         ((ctrl "f"), repeatingArgC $ liftC rightE),
         ((ctrl "g"), liftC $ msgE "Quit"),
         ((ctrl "h"), repeatingArgC $ liftC (leftE >> deleteE)),
--       ((ctrl "i"), indentC),
         ((ctrl "j"), repeatingArgC $ liftC $ insertE '\n'),
--       ((ctrl "k"), killLineC),
         ((ctrl "m"), repeatingArgC $ liftC $ insertE '\n'),
         ((ctrl "n"), repeatingArgC $ liftC downE),
         ((ctrl "o"), repeatingArgC $ liftC (insertE '\n' >> leftE)),
         ((ctrl "p"), repeatingArgC $ liftC upE),
         ((ctrl "q"), repeatingArgC $ insertNextC),
--       ((ctrl "r"), backwardsIncrementalSearchE),
--       ((ctrl "s"), incrementalSearchE),
         ((ctrl "t"), swapC),         
         ((ctrl "u"), readArgC),
--       ((ctrl "v"), scrollDownC),                    
--       ((ctrl "w"), killRegionC),                    
         ((ctrl "x" ++ ctrl "c"), liftC quitE),
         ((ctrl "x" ++ ctrl "s"), liftC fwriteE),
         ((ctrl "x" ++ "o"), liftC nextWinE),
         ((ctrl "x" ++ "k"), liftC closeE),
         ((ctrl "x" ++ "r" ++ "k"), liftC $ msgE "killRect"),
--       ((ctrl "x" ++ "u"), undoC), 
--       ((ctrl "y"), yankC),
--       ((meta "%"), searchReplaceC),
         ((meta "w"), liftC $ msgE "copy"),         
         ([keyLeft], repeatingArgC $ liftC leftE),
         ([keyRight], repeatingArgC $ liftC rightE),
         ([keyUp], repeatingArgC $ liftC upE),
         ([keyDown], repeatingArgC $ liftC downE)
         
        ]


-- | C-t action
swapE :: Action
swapE = do c <- readE
           deleteE
           leftE
           insertE c
           rightE

-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents



withArgC :: (Int -> Command) -> Command
withArgC cmd = do s <- getState
                  cmd (fromMaybe 1 $ esArg s)
                  modifyState $ \s' -> s' { esArg = Nothing }

repeatingArgC :: Command -> Command
repeatingArgC f = withArgC $ \n->replicateM_ n f

insertSelfC :: Command
insertSelfC = repeatingArgC $ do s <- getState
                                 tell $ map insertE (esKey s)

insertNextC :: Command
insertNextC = repeatingArgC $ do c <- readStroke
                                 liftC $ insertE c


-- | C-t action
swapC :: Command
swapC = repeatingArgC $ liftC $ do leftE
                                   c <- readE
                                   deleteE
                                   rightE 
                                   insertE c

-- spawnMinibuffer :: Command -> Command
-- no clue how to do this.


     
-- | Complain about undefined key
undefC :: Command
undefC = do s <- getState
            liftC $ errorE $ "Key sequence not defined : " ++ 
                  showKey (esKey s) ++ " " ++ show (map ord $ esKey s)


-- | C-u stuff
readArgC :: Command
readArgC = do readArg' Nothing

readArg' :: Maybe Int -> Command
readArg' acc = do
    c <- lookStroke
    s <- getState
    if isDigit c
     then (do { readStroke
              ; let acc' = Just $ 10 * (fromMaybe 0 acc) + (ord c - ord '0')
              ; liftC $ msgE (showKey (esKey s) ++ show (fromJust $ acc')) 
              ; readArg' acc'
             }
          )
     else modifyState (\s'->s'{esArg = Just $ fromMaybe 4 acc})


-- * KeyList => keymap
-- Specialized version of MakeKeymap

data KME = KMESubmap KM
         | KMECommand Command

type KM = M.Map Char KME

type KListEnt = ([Char], Command)
type KList = [KListEnt]

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> Command 
makeKeymap kmap = do modifyState (\s -> s {esKey = ""})
                     s <- getState
                     getActions (esKeyMap s)
                     makeKeymap kmap

getActions :: KM -> Command
getActions fm = do
    c <- readStroke
    modifyState (esAddKey c)
    case fromMaybe (KMECommand undefC) (M.lookup c fm) of 
        KMECommand m -> m
        KMESubmap sfm -> do s' <- getState
                            liftC $ msgE (showKey (esKey s') ++ "-")
                            getActions sfm


-- Builds a keymap (Yi.Map.Map) from a key binding list, also creating 
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


-- | entry point
keymap :: [Char] -> [Action]
keymap cs = snd $ 
            runWriter $ 
            flip runStateT (cs, initialState) $ 
            makeKeymap normalKlist
