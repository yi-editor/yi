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
--import Control.Monad.RWS


-- | The state of the editor.
-- Should include much more things
-- (killring, current keylist, etc.)

data ES = ES { esPrecKey :: String, -- ^ should be dropped in favour of esKey
               esKey :: String, 
               esArg :: Maybe Int
             }
          deriving Show

-- | The command type. 
-- Possibly we'll want to switch from CPS to monadic interface someday.

type Command = KProc -> KProc
type KProc = ES -> [Char] -> [Action]


-- * Functions to handle the state.
initialState :: ES
initialState = ES { 
                   esPrecKey = "", -- remove this in favour of esKey
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
         ((ctrl "a"), liftC solE),
         ((ctrl "b"), liftC leftE),
         ((ctrl "d"), liftC deleteE),
         ((ctrl "e"), liftC eolE),
         ((ctrl "f"), liftC rightE),
         ((ctrl "g"), liftC $ msgE "Quit"),
         ((ctrl "h"), liftC (leftE >> deleteE)),
--       ((ctrl "i"), indentC),
--       ((ctrl "k"), killLineC),
         ((ctrl "m"), liftC $ insertE '\n'),
         ((ctrl "n"), liftC downE),
         ((ctrl "o"), liftC (insertE '\n' >> leftE)),
         ((ctrl "p"), liftC upE),
         ((ctrl "q"), insertNextC),
--       ((ctrl "r"), backwardsIncrementalSearchE),
--       ((ctrl "s"), incrementalSearchE),
         ((ctrl "t"), liftC swapE),         
         ((ctrl "u"), readArgC),
--       ((ctrl "v"), pageDownC),                    
--       ((ctrl "w"), killRegionC),                    
         ((ctrl "x" ++ ctrl "c"), liftC quitE),
         ((ctrl "x" ++ ctrl "s"), liftC fwriteE),
         ((ctrl "x" ++ "o"), liftC nextWinE),
         ((ctrl "x" ++ "k"), liftC closeE),
         ((ctrl "x" ++ "r" ++ "k"), liftC $ msgE "killRect"),
--       ((ctrl "x" ++ "u"), undoC), 
--       ((ctrl "y"), yankC),
         ((meta "w"), liftC $ msgE "copy"),
         ([keyLeft], liftC leftE),
         ([keyRight], liftC rightE),
         ([keyUp], liftC upE),
         ([keyDown], liftC downE)
         
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

-- | Convert an Action to a Command
liftC :: Action -> Command
liftC act cont s cs = act:cont s cs


insertSelfC :: Command
insertSelfC cont s cs = map insertE (esPrecKey s) ++ cont s cs

insertNextC :: Command
insertNextC cont s (c:cs) = insertE c : cont s cs
insertNextC _ _ _ = error "dont bug me"



-- | Complain about undefined key
undefC :: Command
undefC cont s cs = (errorE $ "Key sequence not defined : " ++ showKey (esPrecKey s))
                   : cont s cs


-- | C-u stuff
readArgC :: Command
readArgC cont s cs = readArg' cont (s {esArg = Nothing}) cs

readArg' :: Command
readArg' cont s (c:cs) 
    | isDigit c = msgE (showKey (esPrecKey s') ++ show (fromJust $ esArg s')) :
                  readArg' cont s' cs
    | isJust (esArg s) = cont s (c:cs)
    | otherwise = cont (s {esArg = Just 4}) (c:cs)
    where s' = s {esArg = Just $ 10 * (fromMaybe 0 (esArg s)) + (ord c - ord '0')}
readArg' _ _ [] = error "readArg': holy crap"
                       



-- * KeyList => keymap
-- Specialized version of MakeKeymap

data KME = KMESubmap KM
         | KMECommand Command

type KM = M.Map Char KME

type KListEnt = ([Char], Command)
type KList = [KListEnt]

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> KProc 
makeKeymap kmap st cs = actions
    where 
        kfm = buildKeymap M.empty kmap
        kproc = getActions kfm kproc
        actions = kproc st (map remapBS cs)

getActions :: KM -> Command
getActions fm cont s (c:cs) = 
    case fromMaybe (KMECommand undefC) (M.lookup c fm) of 
        KMECommand m -> m cont s'' cs
        KMESubmap sfm -> msgE (showKey (esKey s') ++ "-"):getActions sfm cont s' cs
        
    where s'' = s {esPrecKey = esKey s', esKey = ""}
          s' = esAddKey c s
getActions _ _ _ [] = []


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
keymap = makeKeymap normalKlist initialState
