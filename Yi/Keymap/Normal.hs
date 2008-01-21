{-# OPTIONS -fglasgow-exts #-}
module Yi.Keymap.Normal where

import Control.Monad.State
import Data.Char
import Yi.Keymap.Emacs.Utils (insertSelf)
import Yi.Yi
import qualified Yi.Interact as I (choice, I())

leftHand, rightHand :: [String]
leftHand = ["qwert", "asdfg", "zxcvb"]
rightHand = ["yuiop", "hjkl;", "nm,./"]

{-

qqq www eee rrr ttt yyy uuu iii ooo ppp
 aaa sss ddd fff ggg hhh jjj kkk lll ;;;
  zzz xxx ccc vvv bbb nnn mmm ,,, ... ///

-}

type Process a = (StateT TextUnit (I.I Event Action)) a


-- data Mark = Paste | SetMark | Cut | Copy | SwitchMark
-- data Special = Complete | Undo | Indent | Search

{-
data Action = TextA Direction Unit Operation
            | YiA (YiM ())
            | EditorA
-}

selfInsertKeymap :: Process ()
selfInsertKeymap = do
  Event (KASCII c) [] <- satisfy isPrintableEvent
  write (insertSelf c)
      where isPrintableEvent (Event (KASCII c) []) = c >= ' '
            isPrintableEvent _ = False

retKeymap :: Process ()
retKeymap = do
  Event KEnter [] <- anyEvent
  write (insertSelf '\n')

insertKeymap :: StateT TextUnit (I Event Action) ()
insertKeymap = do
  keyEv 'g'
  many (selfInsertKeymap <|> retKeymap <|> quickCmdKeymap)
  Event KEnter [MCtrl] <- anyEvent
  return ()

quickCmdKeymap :: (MonadInteract m Action Event) => m ()
quickCmdKeymap = I.choice $ concat $ zipWith (zipWith mkCmd) commands rightHand
    where mkCmd cmd ch = do
            Event (KASCII c) [MCtrl] <- anyEvent
            when (c /= ch) $ fail $ "expected " ++ [ch]
            write (cmd Character)

quitKeymap :: Process ()
quitKeymap = do
  keyEv 'q'
  write quitE

unrecognized :: Process ()
unrecognized = do
  e <- anyEvent
  write (msgE $ "unrecognized: " ++ show e)

commandsKeymap :: Process ()
commandsKeymap = quitKeymap <|| (I.choice $ insertKeymap : (concat $ (cmds ++ unts))) <|| unrecognized
    where
      cmds :: [[Process ()]]
      cmds = zipWith (zipWith mkCmd) commands rightHand
      unts = zipWith (zipWith mkUnt) units leftHand
      mkCmd :: (TextUnit -> BufferM ()) -> Char -> Process ()
      mkCmd cmd ch = do
            keyEv ch
            currentUnit <- get
            write (cmd currentUnit)
      mkUnt unt ch = do
        keyEv ch
        put unt

keyEv :: (MonadInteract m w Event) => Char -> m Event
keyEv c = satisfy (== Event (KASCII c) [])

keymap :: Keymap
keymap = runProcess $ (many commandsKeymap >> return ())


{-
Commands: (right hand)


                cop  cut del del com
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
          move dir u = execB Move u dir
          del dir u = deleteB u dir
          xpo dir u = execB Transpose u dir
          b = Backward
          f = Forward
          todo = \_ -> return ()


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

units :: [[TextUnit]]
units = [
         [document, page, column, VLine],
         [Paragraph, Line, Word, Character]
        ]

runProcess :: Process () -> Keymap
runProcess p = fmap fst $ runStateT p Character

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


