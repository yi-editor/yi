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
-- | Y I -- as the name suggests ;) -- uses vi as its default key
-- bindings. Feel free to write your own bindings in ~/.yi/Keymap.hs.
-- You must provide a function 'keymap' of type: Char -> Action
--
-- Contributed by Simon Winwood - http://www.cse.unsw.edu.au/~sjw
--

module Yi.Keymap.Emacs {-( keymap )-} where

import Yi.Yi

import Data.Char
import Control.Monad
import Data.FiniteMap
import Data.IORef
import System.IO.Unsafe -- Yuck

type Key = Char
data KeyTableItem = KAction Action | KMap KeyTable

instance Show KeyTableItem where
    show (KAction _a) = "Action"
    show (KMap _m)    = "Map"

type KeySeq = [Key]

-- type Bind = (Key, KeyTableItem)
type KeyTable = IORef (FiniteMap Key KeyTableItem)
    
nullAction :: KeyTableItem
nullAction = KAction nopE

withKeyTableItem :: KeyTableItem -> (Action -> a) -> (KeyTable -> a) -> a
withKeyTableItem (KAction a) f _ = f a
withKeyTableItem (KMap m)    _ f = f m

copyKeyTable :: KeyTable -> IO (KeyTable)
copyKeyTable k = readIORef k >>= newIORef

newKeyTable :: IO (KeyTable)
newKeyTable = newIORef emptyFM

updateKey ::  KeyTable -> Key -> KeyTableItem -> IO ()
updateKey t k i = modifyIORef t $ \x -> addToFM x k i 

lookupKey :: KeyTable -> Key -> IO (KeyTableItem)
lookupKey t k = (readIORef t >>= \x -> 
		 return $ lookupWithDefaultFM x nullAction k)

{-# NOINLINE globalKeyTable #-}
globalKeyTable :: KeyTable
globalKeyTable = unsafePerformIO newKeyTable

-- Undo/Redo action

-- evil
getPoint = do (_, _, _, _, p, _) <- bufInfoE; return p
setPoint = gotoPoint

saveExcursion :: IO a -> IO a
saveExcursion f = do p <- getPoint; r <- f; setPoint p; return r

data URAction = AC (IO URAction)

deleteA :: Int -> IO URAction
deleteA p = do gotoPoint p
	       c <- readE
	       msgE ("deleteA " ++ show p ++ " " ++ show c)
	       deleteE
	       return (AC $ insertA p c)

insertA :: Int -> Char -> IO URAction
insertA p c = do msgE ("InsertA " ++ show p ++ " " ++ show c)
	         gotoPoint p
		 insertE c
		 return (AC $ deleteA p)

{-# NOINLINE undoRedoState #-}
undoRedoState :: IORef ([URAction], [URAction])
undoRedoState = unsafePerformIO $ newIORef ([], [])

setUndoRedoState :: ([URAction], [URAction]) -> IO ()
setUndoRedoState = writeIORef undoRedoState

doURAction :: IO URAction -> Action
doURAction action = do (undo, redo) <- readIORef undoRedoState
		       u <- action
		       setUndoRedoState (u : undo, []) -- doing something kills the redo state!
		       
undo :: Action
undo = do (undo, redo) <- readIORef undoRedoState
	  case undo of 
		    []     -> msgE "No more undos"
		    ((AC x):xs) -> do r <- x
				      setUndoRedoState (xs, r : redo)

redo :: Action
redo = do (undo, redo) <- readIORef undoRedoState
	  case redo of 
		    []     -> msgE "No more redos"
		    ((AC x):xs) -> do u <- x
				      setUndoRedoState (u : undo, xs)

-- ---------------------------------------------------------------------

control :: Char -> Char
control c = chr (ord '\^A' + (ord (toUpper c) - ord 'A'))

-- ---------------------------------------------------------------------
--
-- This function must be implemented by any user keybinding
--
lookupKeymap :: KeyTable -> Char -> IO Keymap
lookupKeymap _ '\^G' = msgE "Quit" >> return (Keymap $ lookupKeymap globalKeyTable) -- hack
lookupKeymap t k = do key <- lookupKey t k
		      case key of { KAction a -> a >> return (Keymap $ lookupKeymap globalKeyTable);
				    KMap    t' -> return (Keymap $ lookupKeymap t')}
		      
keymap :: Char -> IO Keymap
keymap c = initKeys >> lookupKeymap globalKeyTable c

-- ---------------------------------------------------------------------
--
-- | These functions emulate the XXX-set-key functions
--

keytableSetKey :: KeyTable -> KeySeq -> Action -> IO ()
keytableSetKey _ [] _     = error "Invalid (empty) key sequence"
keytableSetKey t [k] a    = updateKey t k (KAction a)
keytableSetKey t (k:ks) a = do v <- lookupKey t k
			       withKeyTableItem v (\_ -> newKeyTable >>= addMap k ks a) (addMap k ks a)
    where
    addMap k' ks' a' t' = keytableSetKey t' ks' a' >> updateKey t k' (KMap t')

myInsert :: Char -> Action
myInsert c = do p <- getPoint; doURAction (insertA p c)

-- Poor mans key bindings
initKeyBindings :: [([Char], Action)]
initKeyBindings = [-- Special characters
		   ([keyEnter], myInsert '\n'),
		   (['\r'], myInsert '\n'),		   
		   -- Movement
		   ([keyDown], downE),
		   ([keyLeft], leftOrSolE 1),
		   ([keyRight], rightOrEolE 1),
		   ([keyUp], upE),

		   ([control 'a'], solE),
		   ([control 'e'], eolE),
   		   -- Editing
		   ([keyBackspace], leftOrSolE 1  >> deleteE),
		   ([control 'k'], killE),
		   ([control 'd'], deleteE),
		   -- Other
		   ([control 'u'], undo),
		   ([control 'r'], redo),
		   ([control 'x', control 's'], fwriteE),
		   ([control 'x', control 'c'], closeE)
		  ]

initKeys :: IO ()
initKeys = do mapM_ (\c -> keytableSetKey globalKeyTable [c] (myInsert c)) $ 
                            filter isPrint [minBound .. maxBound]
	      mapM_ (uncurry $ keytableSetKey globalKeyTable) initKeyBindings

