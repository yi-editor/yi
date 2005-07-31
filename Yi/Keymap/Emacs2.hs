{-# OPTIONS -cpp -fglasgow-exts #-}
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
import Yi.Yi hiding         ( keymap, meta, string )
import Yi.Window
import Yi.Buffer
import Yi.Keymap.KillRing
import Yi.Char
import qualified Yi.Map as M

import Data.Char           
import Data.Maybe
import Data.List
import Data.Dynamic

import Control.Monad.Writer
import Control.Monad.State

import Text.ParserCombinators.ReadP hiding ( get )

#if __GLASGOW_HASKELL__ < 604
-- just enough parsec for 6.2.2 to build

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

many :: ReadP a -> ReadP [a]
many p = return [] +++ many1 p

many1 :: ReadP a -> ReadP [a]
many1 p = liftM2 (:) p (many p)
#endif

-- * Dynamic state-components

newtype UniversalArg = UniversalArg (Maybe Int)
    deriving Typeable

-- doing the argument precisely is kind of tedious.
-- read: http://www.gnu.org/software/emacs/manual/html_node/Arguments.html
-- and: http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_318.html


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

-- * mark

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

printableChars :: [Char]
printableChars = map chr [32..127]

normalKlist :: KList 
normalKlist = [ ([c], atomic $ insertSelf) | c <- printableChars ] ++
              [
        ("DEL",      atomic $ repeatingArg bdeleteE),
        ("C-SPC",    atomic $ (getPointE >>= setMarkE)),
        ("C-a",      atomic $ repeatingArg solE),
        ("C-b",      atomic $ repeatingArg leftE),
        ("C-d",      atomic $ repeatingArg deleteE),
        ("C-e",      atomic $ repeatingArg eolE),
        ("C-f",      atomic $ repeatingArg rightE),
        ("C-g",      atomic $ msgE "Quit"),
--      ("C-i",      atomic $ indentC),
        ("C-j",      atomic $ repeatingArg $ insertE '\n'),
--        ("C-k",      atomic $ killLineE),
        ("C-m",      atomic $ repeatingArg $ insertE '\n'),
        ("C-n",      atomic $ repeatingArg downE),
        ("C-o",      atomic $ repeatingArg (insertE '\n' >> leftE)),
        ("C-p",      atomic $ repeatingArg upE),
        ("C-q",      insertNextC),
--      ("C-r",      atomic $ backwardsIncrementalSearchE),
--      ("C-s",      atomic $ incrementalSearchE),
        ("C-t",      atomic $ repeatingArg $ swapE),         
        ("C-u",      readArgC),
        ("C-v",      atomic $ scrollDownE),                    
        ("C-w",      atomic $ killRegionE),                    
        ("C-z",      atomic $ suspendE),
        ("C-x ^",    atomic $ repeatingArg enlargeWinE),
        ("C-x C-c",  atomic $ quitE),
        ("C-x C-f",  atomic $ findFile),
        ("C-x C-s",  atomic $ fwriteE),
        ("C-x C-x",  atomic $ exchangePointAndMarkE),
        ("C-x o",    atomic $ nextWinE),
        ("C-x k",    atomic $ closeE),
        ("C-x r k",  atomic $ msgE "killRect"),
        ("C-x u",    atomic $ repeatingArg undoE), 
        ("C-x v",    atomic $ repeatingArg shrinkWinE),
        ("C-y",      atomic $ yankE),
        ("M-<",      atomic $ repeatingArg topE),
        ("M->",      atomic $ repeatingArg botE),
--      ("M-%",      searchReplaceC),
        ("M-DEL",    atomic $ repeatingArg bkillWordE),
        ("M-b",      atomic $ repeatingArg prevWordE),
        ("M-c",      atomic $ repeatingArg capitaliseWordE),
        ("M-d",      atomic $ repeatingArg killWordE),
        ("M-f",      atomic $ repeatingArg nextWordE),
        ("M-l",      atomic $ repeatingArg lowercaseWordE),
        ("M-u",      atomic $ repeatingArg uppercaseWordE),         
        ("M-w",      atomic $ msgE "copy"),         
        ("<left>",   atomic $ repeatingArg leftE),
        ("<right>",  atomic $ repeatingArg rightE),
        ("<up>",     atomic $ repeatingArg upE),
        ("<down>",   atomic $ repeatingArg downE),
        ("<next>",   atomic $ repeatingArg downScreenE),
        ("<prior>",  atomic $ repeatingArg upScreenE)
        ]

-- * Key parser 
-- This really should be in its own module 
-- (importing Text.ParserCombinators.ReadP pollutes the namespace here)

c_ :: Char -> Char
c_ ' ' = '\0'
c_ x = ctrlLowcase x

m_ :: Char -> Char
m_ '\263' = chr 255
m_ x = setMeta x

parseCtrl :: ReadP Char
parseCtrl = do string "C-"
               k <- parseMeta +++ parseRegular
               return $ c_ k

parseMeta :: ReadP Char
parseMeta = do string "M-"
               k <- parseRegular
               return $ m_ k

keyNames :: [(Char, String)]
keyNames = [(' ', "SPC"),
            (keyLeft, "<left>"),
            (keyRight, "<right>"),
            (keyDown, "<down>"),
            (keyUp, "<up>"),
            (keyBackspace, "DEL"),
            (keyNPage, "<next>"),
            (keyPPage, "<prior>")
           ]

parseRegular :: ReadP Char
parseRegular = choice [string s >> return c | (c,s) <- keyNames]
               +++ satisfy (`elem` printableChars)               

parseKey :: ReadP String
parseKey = sepBy1 (parseCtrl +++ parseMeta +++ parseRegular) (munch1 isSpace)

readKey :: String -> String
readKey s = case readKey' s of
              [r] -> r
              rs -> error $ "readKey: " ++ s ++ show (map ord s) ++ " -> " ++ show rs

readKey' :: String -> [String]
readKey' s = map fst $ nub $ filter (null . snd) $ readP_to_S parseKey $ s

-- * Key printer

showKey :: String -> String
showKey = dropSpace . printable'
    where 
        printable' ('\ESC':a:ta) = "M-" ++ [a] ++ printable' ta
        printable' ('\ESC':ta) = "ESC " ++ printable' ta
        printable' (a:ta) 
                | ord a < 32
                = "C-" ++ [chr (ord a + 96)] ++ " " ++ printable' ta
                | isMeta a
                = "M-" ++ printable' (clrMeta a:ta)
                | ord a >= 127
                = bigChar a ++ " " ++ printable' ta 
                | otherwise  = [a, ' '] ++ printable' ta
                
        printable' [] = []

        bigChar c = fromMaybe [c] $ M.lookup c $ M.fromList keyNames


-- * Boilerplate code for the Command monad

liftC :: Action -> KProc ()
liftC = tell . return

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




withUnivArg :: (Maybe Int -> Action) -> Action
withUnivArg cmd = do UniversalArg a <- getDynamic
                     cmd a
                     setDynamic $ UniversalArg Nothing

withIntArg :: (Int -> Action) -> Action
withIntArg cmd = withUnivArg $ \arg -> cmd (fromMaybe 1 arg)

repeatingArg :: Action -> Action
repeatingArg f = withIntArg $ \n->replicateM_ n f

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
keymap = fromKProc $ makeKeymap normalKlist
