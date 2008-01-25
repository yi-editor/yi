-- Copyright (c) 2005, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | An keymap that emulates @mg@, an emacs-like text editor. For more
-- information see <http://www.openbsd.org/cgi-bin/man.cgi?query=mg>
--
-- A quick summary:
--
-- >     ^F     Forward character
-- >     ^B     Backwards character
-- >     ^N     Next line
-- >     ^P     Previous line
-- >     ^A     Start of line
-- >     ^E     End of line
-- >     ^D     delete current character
-- >     ^S     interactive search forward
-- >     ^R     interactive search backwards
-- >     ^O     Open a new line at cursor position
-- >     ^T     transpose characters
-- >     ^U     Repeat next command 4 times (can be cascaded i.e. ^u^u^f will move
-- >            16 characters forward)
-- >
-- >     ^K     kill to end of line (placing into kill buffer)
-- >     ^Y     yank kill buffer into current location
-- >     ^@     set mark
-- >     ^W     kill region (cuts from previously set mark to current location,
-- >            into kill buffer)
-- >     M-W    copy region (into kill buffer)
-- >
-- >     ^V     Next page
-- >     M-V    Previous page
-- >     M-<    start of buffer
-- >     M->    end of buffer
--
-- >     ^X^C   Quit (you will be asked if you want to save files)
-- >     ^X-O   Next window.
-- >     ^X-N   Next window.
-- >     ^X-P   Previous window.
-- >     ^X-U   Undo.
--
-- For more key bindings, type ``M-x describe-bindings''.

module Yi.Keymap.Mg (keymap) where

import Yi.Yi
import Yi.Char

import Numeric              ( showOct )
import Data.Char            ( ord, chr )
import Data.List            ((\\), isPrefixOf)
import qualified Data.Map as M
import Control.Arrow
import Control.Exception    ( try, evaluate )
import Control.Monad
import Control.Monad.Trans
import Yi.Debug
import Yi.Keymap.Emacs.Utils (findFile, isearchProcess, withMinibuffer, completeFileName)
import Yi.Keymap.Emacs.KillRing
import Yi.String (dropSpace)

------------------------------------------------------------------------

c_ :: Char -> Char
c_ = ctrlLowcase

m_ :: Char -> Char
m_ = setMeta

-- ---------------------------------------------------------------------
-- map extended names to corresponding actions
--
extended2action :: M.Map String MgMode
extended2action = M.fromList [ (ex,a) | (ex,_,a) <- globalTable ]

--
-- map keystrokes to extended names
--
keys2extended   :: M.Map [Char] String
keys2extended   = M.fromList [ (k,ex) | (ex,ks,_) <- globalTable, k <- ks ]

--
-- map chars to actions
--
keys2action :: [Char] -> MgMode
keys2action ks | Just ex <- M.lookup ks keys2extended
               , Just a  <- M.lookup ex extended2action = a
               | otherwise = write $ errorE $ "No binding for "++ show ks

--
-- keystrokes only 1 character long
--
unitKeysList :: [Char]
unitKeysList = [ k | (_,ks,_) <- globalTable, [k] <- ks ]

--
-- C-x mappings
--
ctrlxKeysList :: [Char]
ctrlxKeysList = [ k | (_,ks,_) <- globalTable, ['\^X',k] <- ks ]

--
-- M-O mappings
--
metaoKeysList :: [Char]
metaoKeysList = [ k | (_,ks,_) <- globalTable, [m,k] <- ks, m == m_ 'O' ]

------------------------------------------------------------------------
--
-- global key/action/name map
--
globalTable :: [(String,[String],MgMode)]
globalTable = [
  ("apropos",
        [[c_ 'h', 'a']],
        write $ errorE "apropos unimplemented"),
  ("backward-char",
        [[c_ 'b'], [m_ 'O', 'D'], [keyLeft]],
        write $ leftB),
  ("backward-kill-word",
        [[m_ '\127']],
        write $ bkillWordB),
  ("backward-word",
        [[m_ 'b']],
        write $ prevWordB),
  ("beginning-of-buffer",
        [[m_ '<']],
        write $ topB),
  ("beginning-of-line",
        [[c_ 'a'], [m_ 'O', 'H']],
        write $ moveToSol),
  ("call-last-kbd-macro",
        [[c_ 'x', 'e']],
        write $ errorE "call-last-kbd-macro unimplemented"),
  ("capitalize-word",
        [[m_ 'c']],
        write $ capitaliseWordB),
  ("copy-region-as-kill",
        [[m_ 'w']],
        write $ errorE "copy-region-as-kill unimplemented"),
  ("delete-backward-char",
        [['\127'], ['\BS'], [keyBackspace]],
        write $ bdeleteB),
  ("delete-blank-lines",
        [[c_ 'x', c_ 'o']],
        write $ mgDeleteBlanks),
  ("delete-char",
        [[c_ 'd']],
        write $ deleteN 1),
  ("delete-horizontal-space",
        [[m_ '\\']],
        write $ mgDeleteHorizBlanks),
  ("delete-other-windows",
        [[c_ 'x', '1']],
        write $ closeOtherE),
  ("delete-window",
        [[c_ 'x', '0']],
        write $ closeE),
  ("describe-bindings",
        [[c_ 'h', 'b']],
        write $ describeBindings),
  ("describe-key-briefly",
        [[c_ 'h', 'c']],
        describeKeyMode),
  ("digit-argument",
        [ [m_ d] | d <- ['0' .. '9'] ],
        write $ errorE "digit-argument unimplemented"),
  ("dired",
        [[c_ 'x', 'd']],
        write $ errorE "dired unimplemented"),
  ("downcase-region",
        [[c_ 'x', c_ 'l']],
        write $ errorE "downcase-region unimplemented"),
  ("downcase-word",
        [[m_ 'l']],
        write $ lowercaseWordB),
  ("end-kbd-macro",
        [[c_ 'x', ')']],
        write $ errorE "end-kbd-macro unimplemented"),
  ("end-of-buffer",
        [[m_ '>']],
        write $ botB),
  ("end-of-line",
        [[c_ 'e'], [m_ 'O', 'F']],
        write $ moveToEol),
  ("enlarge-window",
        [[c_ 'x', '^']],
        write $ enlargeWinE),
  ("shrink-window",             -- not in mg
        [[c_ 'x', 'v']],
        write $ shrinkWinE),
  ("exchange-point-and-mark",
        [[c_ 'x', c_ 'x']],
        write $ errorE "exchange-point-and-mark unimplemented"),
  ("execute-extended-command",
        [[m_ 'x']],
        metaXSwitch),
  ("fill-paragraph",
        [[m_ 'q']],
        write $ errorE "fill-paragraph unimplemented"),
  ("find-alternate-file",
        [[c_ 'c', c_ 'v']],
        write $ errorE "find-alternate-file unimplemented"),
  ("find-file",
        [[c_ 'x', c_ 'f']],
        findFile),
  ("find-file-other-window",
        [[c_ 'x', '4', c_ 'f']],
        write $ errorE "find-file-other-window unimplemented"),
  ("forward-char",
        [[c_ 'f'], [m_ 'O', 'C'], [keyRight]],
        write $ rightB),
  ("forward-paragraph",
        [[m_ ']']],
        write $ nextNParagraphs 1),
  ("forward-word",
        [[m_ 'f']],
        write $ nextWordB),
  ("goto-line",
        [[c_ 'x', 'g']],
        gotoMode),
  ("help-help",
        [[c_ 'h', c_ 'h']],
        write $ errorE "help-help unimplemented"),
  ("insert-file",
        [[c_ 'x', 'i']],
        write $ errorE "insert-file unimplemented"),
  ("isearch-backward",
        [[c_ 'r']],
        isearchProcess),
  ("isearch-forward",
        [[c_ 's']],
        isearchProcess),
  ("just-one-space",
        [[m_ ' ']],
        write $ insertN ' '),
  ("keyboard-quit",
        [[c_ 'g'],
         [c_ 'h', c_ 'g'],
         [c_ 'x', c_ 'g'],
         [c_ 'x', '4', c_ 'g'],
         [m_ (c_ 'g')]
        ],
        write $ msgE "Quit"),
  ("kill-buffer",
        [[c_ 'x', 'k']],
        killBufferMode),
  ("kill-line",
        [[c_ 'k']],
        write $ killLineE),
  ("kill-region",
        [[c_ 'w']],
        write $ errorE "kill-region unimplemented"),
  ("kill-word",
        [[m_ 'd']],
        write $ killWordB),
  ("list-buffers",
        [[c_ 'x', c_ 'b']],
        write $ mgListBuffers),
  ("negative-argument",
        [[m_ '-']],
        write $ errorE "negative-argument unimplemented"),
  ("newline",
        [[c_ 'm']],
        write $ insertN '\n'),
  ("newline-and-indent",
        [],
        write $ errorE "newline-and-indent unimplemented"),
  ("next-line",
        [[c_ 'n'], [m_ 'O', 'B'], [keyDown]], -- doesn't remember goal column
        write $ (execB Move VLine Forward)),
  ("not-modified",
        [[m_ '~']],
        write $ errorE "not-modified unimplemented"),
  ("open-line",
        [[c_ 'o']],
        write $ insertB '\n'),
  ("other-window",
        [[c_ 'x', 'n'], [c_ 'x', 'o']],
        write $ nextWinE),
  ("previous-line",
        [[c_ 'p'], [m_ 'O', 'A'], [keyUp]],
        write $ (execB Move VLine Backward)),
  ("previous-window",
        [[c_ 'x', 'p']],
        write $ prevWinE),
  ("query-replace",
        [[m_ '%']],
        write $ errorE "query-replace unimplemented"),
  ("quoted-insert",
        [[c_ 'q']],
        insertAnyMode),
  ("recenter",
        [[c_ 'l']],
        write $ errorE "recenter unimplemented"),
  ("save-buffer",
        [[c_ 'x', c_ 's']],
        write $ writeFileMode),
  ("save-buffers-kill-emacs",
        [[c_ 'x', c_ 'c']],
        write $ quitE), -- should ask to save buffers
  ("save-some-buffers",
        [[c_ 'x', 's']],
        write $ errorE "save-some-buffers unimplemented"),
  ("scroll-down",
        [[m_ '[', '5', '~'], [m_ 'v'], [keyPPage]],
        write $ upScreenE),
  ("scroll-other-window",
        [[m_ (c_ 'v')]],
        write $ errorE "scroll-other-window unimplemented"),
  ("scroll-up",
        [[c_ 'v'], [m_ '[', '6', '~'], [keyNPage]],
        write $ downScreenE),
  ("search-backward",
        [[m_ 'r']],
        write $ errorE "search-backward unimplemented"),
  ("search-forward",
        [[m_ 's']],
        write $ errorE "search-forward unimplemented"),
  ("set-fill-column",
        [[c_ 'x', 'f']],
        write $ errorE "set-fill-column unimplemented"),
  ("set-mark-command",
        [['\NUL']],
        write $ errorE "set-mark-command unimplemented"),
  ("split-window-vertically",
        [[c_ 'x', '2']],
        write $ splitE),
  ("start-kbd-macro",
        [[c_ 'x', '(']],
        write $ errorE "start-kbd-macro unimplemented"),
  ("suspend-emacs",
        [[c_ 'z']],
        write $ suspendE),
  ("switch-to-buffer",
        [[c_ 'x', 'b']],
        write $ errorE "switch-to-buffer unimplemented"),
  ("switch-to-buffer-other-window",
        [[c_ 'x', '4', 'b']],
        write $ errorE "switch-to-buffer-other-window unimplemented"),
  ("transpose-chars",
        [[c_ 't']],
        write $ swapB),
  ("undo",
        [[c_ 'x', 'u'], ['\^_']],
        write $ undoB),
  ("universal-argument",
        [[c_ 'u']],
        write $ errorE "universal-argument unimplemented"),
  ("upcase-region",
        [[c_ 'x', c_ 'u']],
        write $ errorE "upcase-region unimplemented"),
  ("upcase-word",
        [[m_ 'u']],
        write $ uppercaseWordB),
  ("what-cursor-position",
        [[c_ 'x', '=']],
        write $ whatCursorPos),
  ("write-file",
        [[c_ 'x', c_ 'w']],
        writeFileMode),
  ("yank",
        [[c_ 'y']],
        write $ getRegE >>= mapM_ insertN) ]

------------------------------------------------------------------------

type MgMode = Interact Char ()

keymap :: Keymap
keymap = comap eventToChar mode

------------------------------------------------------------------------

-- default bindings
mode :: MgMode
mode = command +++
       ctrlxSwitch  +++
       metaSwitch   +++
       metaOSwitch  +++
       metaXSwitch +++ insert

------------------------------------------------------------------------

-- self insertion
insert :: MgMode
insert  = do c <- satisfy (const True); write $ insertN c

-- C- commands
command :: MgMode
command = do c <- oneOf unitKeysList; keys2action [c]

------------------------------------------------------------------------

-- switch to ctrl-X submap
ctrlxSwitch :: MgMode
ctrlxSwitch = do event '\^X' ; write (msgE "C-x-"); ctrlxMode


-- ctrl x submap
ctrlxMode :: MgMode
ctrlxMode = do c <- oneOf ctrlxKeysList; keys2action ['\^X',c]; write msgClrE

------------------------------------------------------------------------
--
-- on escape, we'd also like to switch to M- mode
--

-- switch to meta mode
metaSwitch :: MgMode
metaSwitch = do event '\ESC' ; write  (msgE "ESC-"); metaMode       -- hitting ESC also triggers a meta char

--
-- a fake mode. really just looking up the binding for: m_ c
--
metaMode :: MgMode
metaMode = do c <- oneOf ['\0' .. '\255']       -- not quite right
              when ((m_ c) `elem` unitKeysList) $ keys2action [m_ c]
              write msgClrE

------------------------------------------------------------------------

-- switch to meta O mode
metaOSwitch :: MgMode
metaOSwitch = event (m_ 'O') >> write (msgE "ESC-O-") >> metaOMode

metaOMode :: MgMode
metaOMode = do c <- oneOf metaoKeysList; keys2action [m_ 'O',c]; write msgClrE

-- ---------------------------------------------------------------------
-- build a generic line buffer editor, given a mode to transition to
--

echoMode :: String -> Interact Char (Maybe String)
echoMode prompt = do
  write (logPutStrLn "echoMode")
  result <- lineEdit []
  write msgClrE
  return result
    where lineEdit s =
              do write $ msgE (prompt ++ s)
                 (do delete; lineEdit (take (length s - 1) s)
                  +++ do c <- anyButDelNlArrow; lineEdit (s++[c])
                  +++ do event '\^G'; return Nothing
                  +++ do enter; return (Just s))
          anyButDelNlArrow = oneOf $ any' \\ (enter' ++ delete' ++ ['\ESC',keyUp,keyDown])


withLineEditor :: String -> (String -> MgMode) -> MgMode
withLineEditor prompt cont = do
  s <- echoMode prompt
  case s of
    Nothing -> return ()
    Just x -> cont x


------------------------------------------------------------------------

-- | execute an extended command
-- we ultimately map the command back to a
-- keystroke, and execute that.

metaXSwitch :: MgMode
metaXSwitch = do (event (m_ 'x') +++ event (m_ 'X')); withLineEditor "M-x " metaXEval

-- | M-x mode, evaluate a string entered after M-x
metaXEval :: String -> MgMode
metaXEval cmd = case M.lookup cmd extended2action of
                  Nothing -> write $ msgE "[No match]"
                  Just a  -> a

-- metaXTab :: MgMode

------------------------------------------------------------------------

describeKeyMode :: MgMode
describeKeyMode = describeChar "Describe key briefly: " []

describeChar :: String -> String -> MgMode
describeChar prompt acc = do
  c <- anything
  let keys = acc ++ [c]
  case M.lookup keys keys2extended of
            Just ex -> write $ msgE $ (printable keys) ++ " runs the command " ++ ex
            Nothing ->
                -- only continue if this is the prefix of something in the table
                if any (isPrefixOf keys) (M.keys keys2extended)
                   then do write $ msgE (prompt ++ keys)
                           describeChar prompt keys
                   else write $ msgE $ printable keys ++ " is not bound to any function"

-- ---------------------------------------------------------------------
-- | Writing a file
--

writeFileMode :: MgMode
writeFileMode = withMinibuffer "Write file: "
                                          (completeFileName Nothing)
                                          fwriteToE

-- ---------------------------------------------------------------------
-- | Killing a buffer by name

killBufferMode :: MgMode
killBufferMode = withLineEditor "Kill buffer: " $ \buf -> write $ do
                   closeBufferE buf

-- ---------------------------------------------------------------------
-- | Goto a line
--

gotoMode :: MgMode
gotoMode = withLineEditor "goto line: " $ \l -> write $ do
             i <- lift $ try . evaluate . read $ l
             case i of Left _   -> errorE "Invalid number"
                       Right i' -> gotoLn i'


-- | insert the first character, then switch back to normal mode
insertAnyMode :: MgMode
insertAnyMode = do c <- oneOf ['\0' .. '\255']; write (insertN c)

-- | translate a string into the Emacs encoding of that string
printable :: String -> String
printable = dropSpace . printable'
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

        bigChar c
                | c == keyDown  = "<down"
                | c == keyUp    = "<up>"
                | c == keyLeft  = "<left>"
                | c == keyRight = "<right>"
                | c == keyNPage = "<pagedown>"
                | c == keyPPage = "<pageup>"
                | c == '\127'   = "<delete>"
                | otherwise     = show c

------------------------------------------------------------------------
-- Mg-specific actions

whatCursorPos :: Action
whatCursorPos = do
        bufInfo <- bufInfoB
        let ln  = bufInfoLineNo  bufInfo
            col = bufInfoColNo   bufInfo
            pt  = bufInfoCharNo  bufInfo
            pct = bufInfoPercent bufInfo
        c <- readB
        msgE $ "Char: "++[c]++" (0"++showOct (ord c) ""++
                ")  point="++show pt++
                "("++pct++
                ")  line="++show ln++
                "  row=? col="++ show col

describeBindings :: Action
describeBindings = newBufferE "*help*" s >> return ()
    where
      s = unlines [ let p = printable k
                    in p ++ replicate (17 - length p) ' ' ++ ex
                  | (ex,ks,_) <- globalTable
                  , k         <- ks ]

-- bit of a hack, unfortunately
mgListBuffers :: Action
mgListBuffers = do
        closeBufferE name   -- close any previous buffer list buffer
        newBufferE name []  -- new empty one
        bs  <- listBuffersE -- get current list
        closeBufferE name   -- close temporary one
        newBufferE name (f bs) -- and finally display current one
        return ()
    where
        name = "*Buffer List*"
        f bs = unlines [ "  "++(show i)++"\t"++(show n) | (n,i) <- bs ]

--
-- delete all blank lines from this point
mgDeleteBlanks :: Action
mgDeleteBlanks = do
        p <- getSelectionMarkPointB
        moveWhileE (== '\n') GoRight
        q <- getSelectionMarkPointB
        gotoPointE p
        deleteNE (q - p)

-- not quite right, as it will delete, even if no blanks
mgDeleteHorizBlanks :: Action
mgDeleteHorizBlanks = do
        p <- getSelectionMarkPointB
        moveWhileE (\c -> c == ' ' || c == '\t') GoRight
        q <- getSelectionMarkPointB
        gotoPointE p
        deleteNE (q - p)

------------------------------------------------------------------------
--
-- some regular expressions

any', enter', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']

delete, enter, anything :: Interact Char Char
delete  = oneOf delete'
enter   = oneOf enter'
anything  = oneOf any'

