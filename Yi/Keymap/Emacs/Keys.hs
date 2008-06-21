-- Copyright (c) 2005, 2008 Jean-Philippe Bernardy

-- This is an attempt at mimicing emacs names for keys. This is
-- deprecated in favour of proper key-construction combinators.  They
-- are currently being developped as part of the CUA keybindings.

module Yi.Keymap.Emacs.Keys 
  ( readKey
  , showKey
  , printableChars
  , KList
  , makeKeymap
  , makePartialKeymap
  , rebind
  ) 
where
    
import Yi.Event
import Yi.Debug
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Prelude hiding (error)
import Text.ParserCombinators.ReadP
import Yi.Keymap
import qualified Yi.Interact as I

-- * The keymap abstract definition

type KList = [(String, Keymap)]

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> KeymapM ()
makeKeymap kmap = I.choice [I.events (readKey k) >> a | (k,a) <- kmap]


-- | Creates a binding in the same way as 'makeKeymap' however the
-- second argument is what to map all unrecognised events to.
makePartialKeymap :: KList -> Keymap -> KeymapM ()
makePartialKeymap kmap emptymap =
  (makeKeymap kmap) I.<|| emptymap

rebind :: KList -> KeymapEndo
rebind keys = (makeKeymap keys I.<||)


printableChars :: [Char]
printableChars = map chr [32..127]


-- * Key parser

x_ :: [Modifier] -> Event -> Event
x_ mods' (Event k mods) = Event k (nub (mods'++mods))

parseShift :: ReadP Event
parseShift = do string "S-"
                k <- parseRegular
                return $ x_ [MShift] k

parseCtrl :: ReadP Event
parseCtrl = do string "C-"
               k <- parseRegular
               return $ x_ [MCtrl] k

parseMeta :: ReadP Event
parseMeta = do string "M-"
               k <- parseRegular
               return $ x_ [MMeta] k

parseCtrlMeta :: ReadP Event
parseCtrlMeta = do string "C-M-"
                   k <- parseRegular
                   return $ x_ [MMeta, MCtrl] k


keyNames :: [(Key, String)]
keyNames = [(KASCII ' ', "SPC"),
	    (KTab, "TAB"),
            (KLeft, "<left>"),
            (KRight, "<right>"),
            (KDown, "<down>"),
            (KUp, "<up>"),
            (KDel, "DEL"),
            (KBS, "BACKSP"),
            (KPageDown, "<next>"),
            (KPageUp, "<prior>"),
            (KHome, "<home>"),
            (KEnd, "<end>"),
            (KEnter, "RET")
           ]

parseRegular :: ReadP Event
parseRegular = choice [string s >> return (Event c []) | (c,s) <- keyNames]
               +++ do c <- satisfy (`elem` printableChars)
                      return (Event (KASCII c) [])

parseKey :: ReadP [Event]
parseKey = sepBy1 (choice [parseCtrlMeta, parseCtrl, parseMeta, parseShift, parseRegular])
                  (munch1 isSpace)

readKey :: String -> [Event]
readKey s = case readKey' s of
              [r] -> r
              rs -> error $ "readKey: " ++ s ++ show (map ord s) ++ " -> " ++ show rs

readKey' :: String -> [[Event]]
readKey' s = map fst $ nub $ filter (null . snd) $ readP_to_S parseKey $ s

-- * Key printer
-- FIXME: C- and M- should be swapped when they are both there.
showKey :: [Event] -> String
showKey = concat . intersperse " " . map showEv
    where
      showEv (Event k mods) = concatMap showMod mods ++ showK k
      showMod MCtrl = "C-"
      showMod MShift = "S-"
      showMod MMeta = "M-"

      showK (KASCII x) = [x]
      showK c = fromJust $ M.lookup c $ M.fromList keyNames


