{-# LANGUAGE FlexibleContexts #-}
module Yi.Users.JP.Experimental where
-- This is an attempt at a completely "normalized" keymap.
-- Choose your mode/unit with the left hand;
-- Perform commands with the right hand.

import Prelude (zipWith)
import Control.Monad.State
import Data.Char
import Yi.Keymap.Emacs.Utils
import Yi.Rectangle
import Yi
import qualified Yi.Interact as I (choice, I())

-- | Enhanced keymap type, where the current unit is remembered using a StateT
type KM a = (StateT (TextUnit, String) (I.I Event Action)) a


{-
  We'll assume QWERTY layout:

qqq www eee rrr ttt yyy uuu iii ooo ppp
 aaa sss ddd fff ggg hhh jjj kkk lll ;;;
  zzz xxx ccc vvv bbb nnn mmm ,,, ... ///
-}

-- | Keyboard layout definition
leftHand, rightHand :: [String]
leftHand = ["qwert", "asdfg", "zxcvb"]
rightHand = ["yuiop", "hjkl;", "nm,./"]

-- data Mark = Paste | SetMark | Cut | Copy | SwitchMark
-- data Special = Complete | Undo | Indent | Search

-- Special shift for events that understands qwerty layout.
shi_ :: Event ->Event
shi_ (Event (KASCII c) ms) | isAlpha c = Event (KASCII (toUpper c)) ms
shi_ (Event (KASCII ',') ms)  = Event (KASCII '<') ms
shi_ (Event (KASCII '.') ms)  = Event (KASCII '>') ms
shi_ (Event (KASCII '/') ms)  = Event (KASCII '?') ms
shi_ (Event (KASCII ';') ms)  = Event (KASCII ':') ms
shi_ (Event (KASCII '\'') ms)  = Event (KASCII '"') ms
shi_ (Event (KASCII '[') ms)  = Event (KASCII '{') ms
shi_ (Event (KASCII ']') ms)  = Event (KASCII '}') ms
shi_ _ = error "shi_: unhandled event"                          


selfInsertKeymap :: KM ()
selfInsertKeymap = do
  c <- printableChar
  write (insertB c)

retKeymap :: KM ()
retKeymap = do
  Event KEnter [] <- anyEvent
  write (insertB '\n')

insertKeymap :: KM ()
insertKeymap = do
  event $ char 'g'
  write $ msgEditor "-- INSERT --"
  many $ do
    write $ msgEditor "-- INSERT --"
    (selfInsertKeymap <|> retKeymap <|> quickCmdKeymap) <|| unrecognized
  quitInsert
  return ()

quitInsert :: KM Event
quitInsert = oneOf [ctrl $ spec KEnter, spec KEsc, ctrlCh '\\']

quickCmdKeymap :: KM ()
quickCmdKeymap =     mkCmdKeymap (return Character)  ctrl
                 <|> mkCmdKeymap (return unitWord)   (ctrl . shi_)

quitKeymap :: KM ()
quitKeymap = do
  Event KEsc [] <- anyEvent
  write quitEditor

unrecognized :: KM ()
unrecognized = do
  e <- anyEvent
  write (msgEditor $ "unrecognized: " ++ show e)

commandsKeymap :: KM ()
commandsKeymap = do
  (_, unitName) <- get
  write $ msgEditor $ "-- CMD: " ++ unitName
  quitKeymap <|| (I.choice $ insertKeymap : cmds : concat unts)
    where
      cmds = mkCmdKeymap (fst <$> get) id
      unts = zipWith (zipWith mkUnt) units leftHand
      mkUnt unt ch = do
        event $ char ch
        put unt

mkCmdKeymap :: KM TextUnit -> (Event -> Event) -> KM ()
mkCmdKeymap getUnit mods = I.choice $ concat $ zipWith (zipWith mkCmd) commands rightHand
    where mkCmd cmd ch = do
            event $ mods $ char ch
            unt <- getUnit
            write (cmd unt)

keymap :: Keymap
keymap = runKM $ forever $ choice 
         [ 
          metaCh 'x' ?>>! executeExtendedCommandE,
          commandsKeymap,
          ctrlCh 'x' ?>> ctrlX
         ]


{-
Commands: (right hand)


                cop  cut del del com ???
                 pop  pas mov mov sea '''
                  mpp  mxp xpo xpo und

com: complete
und: undo
sea: start incremental search of the Unit at point
pop: pop-yank
pas: paste
xpo: transpose in given direction
''': search start from empty
mxp: exchange point and mark
mpp: mark pop
cop: copy

-}

commands :: [[TextUnit -> BufferM ()]]
commands = [[copy, cut,   del  b, del f,  complete],
            [pop,  paste, move b, move f, search],
            [mpp,  mxp,   xpo  b, xpo  f, undo]]
    where copy = todo
          cut = todo
          pop = todo
          mpp = todo
          mxp = todo
          complete = todo
          paste = todo
          search = todo
          undo = const undoB
          move dir u = moveB u dir
          del dir u = deleteB u dir
          xpo dir u = transposeB u dir
          b = Backward
          f = Forward
          todo = const $ return ()


{-
Units: (left hand)


doc pag col ver   ovr
 par lin wor cha   ins
  *** *** *** sea   buf
-}

document, page, column :: TextUnit
document = Character
page = Character
column = Character

units :: [[(TextUnit, String)]]
units = [
         [(document, "DOC"), (page, "PAGE"), (column, "COL"), (VLine, "VER")], -- ↕
         [(unitParagraph, "PARA"), (Line, "Line"), (unitWord, "Word"), (Character, "Char")]
        ]

runKM :: KM () -> Keymap
runKM p = fmap fst $ runStateT p (Character, "Char")

{-
ins: go to insert mode
ovr: go to overwrite mode
sea: navigate searched items.


... free
*** reserved for normal emacs usage.

----------


C-: briefly switch to character mode
M-: briefly switch to word mode

C-mode: go to that mode

-}




------------
-- C-x commands borrowed from emacs.

ctrlX :: KM ()
ctrlX = 
        choice [ ctrlCh 'o'    ?>>! deleteBlankLinesB
               , char '0'      ?>>! closeWindow
               , char '1'      ?>>! closeOtherE
               , char '2'      ?>>! splitE
               , char 's'      ?>>! askSaveEditor
               , ctrlCh 'c'    ?>>! askQuitEditor
               , ctrlCh 'f'    ?>>! findFile
               , ctrlCh 's'    ?>>! fwriteE
               , ctrlCh 'w'    ?>>! promptFile "Write file:" fwriteToE
               , ctrlCh 'x'    ?>>! (exchangePointAndMarkB >> 
                                     putA highlightSelectionA True)
               , char 'b'      ?>>! switchBufferE
               , char 'd'      ?>>! dired
               , char 'e' ?>> 
                 char 'e'      ?>>! evalRegionE
               , char 'o'      ?>>! nextWinE
               , char 'k'      ?>>! killBufferE
               , char 'r'      ?>> rectangleFuntions
               , char 'u'      ?>>! undoB
               , char 'v'      ?>>! shrinkWinE
               ]

rectangleFuntions :: KM ()
rectangleFuntions = choice [char 'a' ?>>! alignRegionOn,
                            char 'o' ?>>! openRectangle,
                            char 't' ?>>! stringRectangle,
                            char 'k' ?>>! killRectangle,
                            char 'y' ?>>! yankRectangle
                            ]
