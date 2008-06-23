{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--


-- | Vim keymap for Yi. Emulates vim :set nocompatible
module Yi.Keymap.Vim ( keymap, VimMode, viWrite ) where

import Yi.Yi
import Yi.Prelude
import Prelude (maybe, length, filter, map, drop, takeWhile, dropWhile, break)

import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.Dynamic

import Control.Exception    ( ioErrors, try, evaluate )
import Control.Monad.State hiding (mapM_, mapM)

import Yi.Editor
import Yi.History
import Yi.Buffer
import Yi.Debug

import Yi.Indent

import Yi.Keymap.Emacs.Utils (completeFileName,completeBufferName)
import Yi.MiniBuffer
import Yi.TextCompletion
import Yi.Keymap.Keys

--
-- What's missing?
--   fancier :s//
--   '.'
--   movement parameterised \> \<
--

-- ---------------------------------------------------------------------

type VimMode = KeymapM ()


-- | Top level
keymap :: Keymap
keymap = cmd_mode


------------------------------------------------------------------------

-- The Vim keymap is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate VimProcs for each phase of key input in that mode.

-- | Command mode consists of simple commands that take a count arg,
-- the replace cmd, which consumes one char of input, and commands
-- that switch modes.
cmd_mode :: VimMode
cmd_mode = choice [cmd_eval,cmd_move,cmd2other,cmd_op]


-- | Leave a mode. This always has priority over catch-all actions inside the mode.
leave :: VimMode
leave = spec KEsc ?>> adjustPriority (-1) (write msgClr)

-- | Insert mode is either insertion actions, or the meta (\ESC) action
ins_mode :: VimMode
ins_mode = write (setStatus "-- INSERT --") >> many (ins_char <|> kwd_mode) >> leave

-- | Replace mode is like insert, except it performs writes, not inserts
rep_mode :: VimMode
rep_mode = write (setStatus "-- REPLACE --") >> many rep_char >> leave

-- | Visual mode, similar to command mode
vis_mode :: SelectionStyle -> VimMode
vis_mode selectionStyle = do
  write (withBuffer (setVisibleSelection True >> pointB >>= setSelectionMarkPointB))
  core_vis_mode selectionStyle
  write (msgClr >> withBuffer0 (setVisibleSelection False) >> withBuffer0 (setDynamicB $ SelectionStyle Character))

core_vis_mode :: SelectionStyle -> VimMode
core_vis_mode selectionStyle = do
  write $ withEditor $ do withBuffer0 $ setDynamicB $ selectionStyle
                          setStatus $ msg selectionStyle
  many cmd_move
  (vis_single selectionStyle <|| vis_multi)
  where msg (SelectionStyle Line) = "-- VISUAL LINE --"
        msg (SelectionStyle _)    = "-- VISUAL --"

-- | Change visual mode
change_vis_mode :: SelectionStyle -> SelectionStyle -> VimMode
change_vis_mode (SelectionStyle Character) (SelectionStyle Character) = return ()
change_vis_mode (SelectionStyle Line)      (SelectionStyle Line)      = return ()
change_vis_mode _                          dst                        = core_vis_mode dst


-- | A KeymapM to accumulate digits.
-- typically what is needed for integer repetition arguments to commands
count :: KeymapM (Maybe Int)
count = option Nothing $ do
    c <- charOf id '1' '9'
    cs <- many $ charOf id '0' '9'
    return $ Just $ read (c:cs)

data RegionStyle = LineWise
                 | Inclusive
                 | Exclusive
  deriving (Eq, Typeable, Show)

data ViMove = Move TextUnit Direction
            | MaybeMove TextUnit Direction
            | GenMove TextUnit (Direction, BoundarySide) Direction
            | CharMove Direction
            | ArbMove (BufferM ())
            | Replicate ViMove Int
            | SeqMove ViMove ViMove
            | NoMove

viMoveToEol :: ViMove
viMoveToEol = MaybeMove Line Forward

viMoveToSol :: ViMove
viMoveToSol = MaybeMove Line Backward

selection2regionStyle :: SelectionStyle -> RegionStyle
selection2regionStyle (SelectionStyle Line)      = LineWise
selection2regionStyle (SelectionStyle Character) = Inclusive
selection2regionStyle _                          = error "selection2regionStyle"

-- ---------------------------------------------------------------------
-- | KeymapM for movement commands
--
-- The may be invoked directly, or sometimes as arguments to other
-- /operator/ commands (like d).
--

pString :: String -> KeymapM [Event] 
pString = events . map char

cmd_move :: VimMode
cmd_move = gen_cmd_move >>= write . viMove . snd

-- the returned RegionStyle is used when the movement is combined with a 'cut' or 'yank'.
gen_cmd_move :: KeymapM (RegionStyle, ViMove)
gen_cmd_move = do
  cnt <- count
  let x = maybe 1 id cnt
  choice ([c ?>> return (Inclusive, a x) | (c,a) <- moveCmdFM_inclusive, (c /= char '0' || Nothing == cnt) ] ++
          [c ?>> return (Exclusive, a x) | (c,a) <- moveCmdFM_exclusive ] ++
          [c ?>> return (LineWise, a x) | (c,a) <- moveUpDownCmdFM] ++
          [do event c; c' <- textChar; return (r, a c' x) | (c,r,a) <- move2CmdFM] ++
          [char 'G' ?>> return (LineWise, ArbMove (case cnt of
                                                     Nothing -> botB >> moveToSol
                                                     Just n  -> gotoLn n >> return ()))
          ,pString "gg" >> return (LineWise, ArbMove (gotoLn 0 >> return ()))
          ,pString "ge" >> return (Inclusive, Replicate (GenMove ViWord (Forward, InsideBound) Backward) x)])

-- | movement commands (with exclusive cut/yank semantics)
moveCmdFM_exclusive :: [(Event, (Int -> ViMove))]
moveCmdFM_exclusive =
-- left/right
    [(char 'h',        left)
    ,(ctrl $ char 'h', left)
    ,(spec KBS,        left)
    ,(spec KLeft,      left)
    ,(spec KRight,     right)
    ,(char 'l',        right)
    ,(char ' ',        right)
    ,(spec KHome,      sol)
    ,(char '0',        sol)
    ,(char '^',        const $ ArbMove firstNonSpaceB)
    ,(char '|',        \i -> SeqMove viMoveToSol (Replicate (CharMove Forward) (i-1)))

-- words
    ,(char 'w',       Replicate $ GenMove ViWord (Backward,InsideBound) Forward)
    ,(char 'b',       Replicate $ GenMove ViWord (Backward,InsideBound) Backward)
-- text
    ,(char '{',       Replicate $ Move Paragraph Backward)
    ,(char '}',       Replicate $ Move Paragraph Forward)
    ]
    where
        left  = Replicate $ CharMove Backward
        right = Replicate $ CharMove Forward
        sol   = Replicate $ viMoveToSol

-- | movement commands (with inclusive cut/yank semantics)
moveCmdFM_inclusive :: [(Event, (Int -> ViMove))]
moveCmdFM_inclusive =
-- left/right
    [(char '$',  eol)
    ,(spec KEnd, eol)
-- words
    ,(char 'e',  Replicate $ GenMove ViWord (Forward, InsideBound) Forward)]
    where
        eol   = Replicate $ viMoveToEol


viMove :: ViMove -> BufferM ()
viMove NoMove                              = return ()
viMove (GenMove   unit boundary direction) = genMoveB unit boundary direction
viMove (MaybeMove unit          direction) = maybeMoveB unit direction
viMove (Move      unit          direction) = moveB unit direction
viMove (CharMove Forward)                  = moveXorEol 1
viMove (CharMove Backward)                 = moveXorSol 1
viMove (ArbMove       move)                = move
viMove (SeqMove move1 move2)               = viMove move1 >> viMove move2
viMove (Replicate     move i)              = viReplicateMove move i

viReplicateMove :: ViMove -> Int -> BufferM ()
viReplicateMove (Move VLine Forward)  i = lineMoveRel i >> return ()
viReplicateMove (Move VLine Backward) i = lineMoveRel (-i) >> return ()
viReplicateMove (CharMove Forward)    i = moveXorEol i
viReplicateMove (CharMove Backward)   i = moveXorSol i
viReplicateMove move                  i = replicateM_ i $ viMove move


-- | up/down movement commands. these one are separated from moveCmdFM_{inclusive,exclusive}
-- because they behave differently when yanking/cuting (line mode).
moveUpDownCmdFM :: [(Event, Int -> ViMove)]
moveUpDownCmdFM =
    [(char 'k',        up)
    ,(spec KUp,        up)
    ,(ctrl $ char 'p', up)
    ,(char 'j',        down)
    ,(spec KDown,      down)
    ,(ctrl $ char 'j', down)
    ,(ctrl $ char 'n', down)
    ,(spec KEnter,     down)
-- misc
    ,(char 'H',        \i -> ArbMove (downFromTosB (i - 1)))
    ,(char 'M',        const $ ArbMove middleB)
    ,(char 'L',        \i -> ArbMove (upFromBosB (i - 1)))
    ]
    where
        up   = Replicate (Move VLine Backward)
        down = Replicate (Move VLine Forward)

--  | more movement commands. these ones are paramaterised by a character
-- to find in the buffer.
move2CmdFM :: [(Event, RegionStyle, Char -> Int -> ViMove)]
move2CmdFM =
    -- these Inc/Exc in {next,prev}C{Inc,Exc} are not quite the same
    -- than Exclusive/Inclusive, look at the vim manual for more details.
    [(char 'f', Inclusive, Replicate . ArbMove . nextCInc)
    ,(char 'F', Exclusive, Replicate . ArbMove . prevCInc)
    ,(char 't', Inclusive, Replicate . ArbMove . nextCExc)
    ,(char 'T', Exclusive, Replicate . ArbMove . prevCExc)
    ]

-- | Other command mode functions
cmd_eval :: VimMode
cmd_eval = do
   cnt <- count
   let i = maybe 1 id cnt
   choice
    ([c ?>>! action i | (c,action) <- singleCmdFM ] ++
     [events evs >>! action i | (evs, action) <- multiCmdFM ] ++
     [char 'r' ?>> textChar >>= write . writeB])

-- TODO: escape the current word
--       at word bounds: search for \<word\>
searchCurrentWord :: YiM ()
searchCurrentWord = do
  w <- withBuffer $ readRegionB =<< regionOfB ViWord
  doSearch (Just w) [] Forward

-- | Parse any character that can be inserted in the text.
textChar :: KeymapM Char
textChar = do
  Event (KASCII c) [] <- anyEvent
  return c

continueSearching :: Direction -> YiM ()
continueSearching direction = do
  withEditor $ getRegexE >>= printMsg . ("/" ++) . fst . fromMaybe ([],undefined)
  doSearch Nothing [] direction

-- | cmd mode commands
singleCmdFM :: [(Event, Int -> YiM ())]
singleCmdFM =
    [(ctrl $ char 'b',    withBuffer . upScreensB)             -- vim does (firstNonSpaceB;moveXorSol)
    ,(ctrl $ char 'f',    withBuffer . downScreensB)
    ,(ctrl $ char 'g',    const viFileInfo)        -- hmm. not working. duh. we clear
    ,(ctrl $ char 'l',    const refreshEditor)
    ,(ctrl $ char 'r',    withBuffer . flip replicateM_ redoB)
    ,(ctrl $ char 'z',    const suspendEditor)
    ,(char 'D',      const (withEditor $ withBuffer0 readRestOfLnB >>= setRegE >> withBuffer0 deleteToEol))
    ,(char 'J',      const (withBuffer (moveToEol >> deleteN 1)))    -- the "\n"
    ,(char 'U',      withBuffer . flip replicateM_ undoB)    -- NB not correct
    ,(char 'n',      const $ continueSearching Forward)
    ,(char 'u',      withBuffer . flip replicateM_ undoB)

    ,(char 'X',      \i -> withBuffer $ do 
                             p <- pointB
                             moveXorSol i
                             q <- pointB
                             deleteNBytes (p ~- q) q)

    ,(char 'x',      \i -> withBuffer $ do 
                             p <- pointB -- not handling eol properly
                             moveXorEol i
                             q <- pointB
                             deleteNBytes (q ~- p) p)

    ,(char 'p',      withEditor . flip replicateM_ pasteAfter)

    ,(char 'P',      withEditor . flip replicateM_ pasteBefore)

    ,(spec KPageUp, withBuffer . upScreensB)
    ,(spec KPageDown, withBuffer . downScreensB)
    ,(char '*',      const $ searchCurrentWord)
    ,(char '~',      \i -> withBuffer $ do
                         p <- pointB
                         moveXorEol i
                         q <- pointB
                         moveTo p
                         mapRegionB (mkRegion p q) $ \c ->
                             if isUpper c then toLower c else toUpper c
                         moveTo q)
    ]

ctrlW :: Event
ctrlW = ctrl $ char 'w'

multiCmdFM :: [([Event], Int -> YiM ())]
multiCmdFM =
    [([ctrlW, char 'c'], const $ withEditor tryCloseE)
    ,([ctrlW, char 'o'], const $ withEditor closeOtherE)
    ,([ctrlW, char 's'], const $ withEditor splitE)
    ,([ctrlW, char 'w'], const $ withEditor nextWinE)
    ,([ctrlW, char 'W'], const $ withEditor prevWinE)
    ,([ctrlW, char 'p'], const $ withEditor prevWinE)

    -- since we don't have vertical splitting,
    -- these moving can be done using next/prev.
    ,([ctrlW,spec KDown], const $ withEditor nextWinE)
    ,([ctrlW,spec KUp], const $ withEditor prevWinE)
    ,([ctrlW,spec KRight], const $ withEditor nextWinE)
    ,([ctrlW,spec KLeft], const $ withEditor prevWinE)
    ,([ctrlW,char 'k'],   const $ withEditor prevWinE)
    ,([ctrlW,char 'j'],   const $ withEditor nextWinE)    -- Same as the above pair, when you're a bit slow to release ctl.
    ,([ctrlW, ctrl $ char 'k'], const $ withEditor prevWinE)
    ,([ctrlW, ctrl $ char 'j'], const $ withEditor nextWinE)
    ,(map char ">>", withBuffer . shiftIndentOfLine)
    ,(map char "<<", withBuffer . shiftIndentOfLine . negate)
    ,(map char "ZZ", const $ viWrite >> quitEditor)
    ]

-- | So-called 'operators', which take movement actions as arguments.
--
-- How do we achive this? We parse a known operator char then parse
-- one of the known movement commands.  We then apply the returned
-- action and then the operator. For example, we 'd' command stores
-- the current point, does a movement, then deletes from the old to
-- the new point.
cmd_op :: VimMode
cmd_op = do
  cnt <- count
  let i = maybe 1 id cnt
  choice $ [pString "dd" >>! cut  (Replicate (Move VLine Forward) (i-1)) LineWise,
            pString "yy" >>! yank (Replicate (Move VLine Forward) (i-1)) LineWise,
            char 'Y' ?>>! yank (Replicate (Move VLine Forward) (i-1)) LineWise] ++
           [do event (char c)
               (regionStyle, m) <- gen_cmd_move
               write $ a (Replicate m i) regionStyle
           | (c,a) <- opCmdFM]
    where
        -- | operator (i.e. movement-parameterised) actions
        opCmdFM :: [(Char, ViMove -> RegionStyle -> EditorM ())]
        opCmdFM =  [('d', cut), ('y', yank)]

lineWiseRegion :: Point -> Point -> BufferM Region
lineWiseRegion start' stop' = savingPointB $ do
  moveTo start'
  maybeMoveB Line Backward
  start <- pointB
  moveTo stop'
  maybeMoveB Line Forward
  stop <- pointB
  return $ mkVimRegion start stop

regionFromTo :: Point -> ViMove -> RegionStyle -> BufferM Region
regionFromTo start' move regionStyle = do
  stop' <- savingPointB (viMove move >> pointB)
  let [start, stop] = sort [start', stop']
  case regionStyle of
    LineWise -> lineWiseRegion start stop
    Inclusive -> return $ mkRegion start (stop + 1)
    Exclusive -> return $ mkRegion start stop

onSelection :: (Point -> ViMove -> RegionStyle -> EditorM ()) -> EditorM ()
onSelection op = do
  start <- withBuffer0 getSelectionMarkPointB
  op start NoMove . selection2regionStyle =<< withBuffer0 getDynamicB

yankFrom :: Point -> ViMove -> RegionStyle -> EditorM ()
yankFrom start move regionStyle = do
  txt <- withBuffer0 $ do
    p <- pointB
    region <- regionFromTo start move regionStyle
    moveTo p
    readRegionB region
  setRegE $ if (regionStyle == LineWise) then '\n':txt else txt
  let rowsYanked = length (filter (== '\n') txt)
  when (rowsYanked > 2) $ printMsg $ (show rowsYanked) ++ " lines yanked"

yank :: ViMove -> RegionStyle -> EditorM ()
yank move regionStyle = do start <- withBuffer0 pointB
                           yankFrom start move regionStyle

yankSelection :: EditorM ()
yankSelection = onSelection yankFrom

cutFrom :: Point -> ViMove -> RegionStyle -> EditorM ()
cutFrom start move regionStyle = do
  txt <- withBuffer0 $ do
    region <- regionFromTo start move regionStyle
    txt <- readRegionB region
    deleteRegionB region
    return txt
  setRegE $ if (regionStyle == LineWise) then '\n':txt else txt
  let rowsCut = length (filter (== '\n') txt)
  when (rowsCut > 2) $ printMsg ( (show rowsCut) ++ " fewer lines")

cut :: ViMove -> RegionStyle -> EditorM ()
cut move regionStyle = do start <- withBuffer0 pointB
                          cutFrom start move regionStyle

cutSelection :: EditorM ()
cutSelection = onSelection cutFrom

pasteOverSelection :: EditorM ()
pasteOverSelection = do
  txt <- getRegE
  withBuffer0 $ do
    selectionStyle <- getDynamicB
    start <- getSelectionMarkPointB
    region <- regionFromTo start NoMove $ selection2regionStyle $ selectionStyle
    moveTo $ regionStart region
    deleteRegionB region
    insertN txt

pasteAfter :: EditorM ()
pasteAfter = do
  txt' <- getRegE
  withBuffer0 $
    case txt' of
      '\n':txt -> moveToEol >> rightB >> insertN txt >> leftN (length txt)
      _      -> moveXorEol 1 >> insertN txt' >> leftB

pasteBefore :: EditorM ()
pasteBefore = do
  txt' <- getRegE
  withBuffer0 $ do
    case txt' of
      '\n':txt -> moveToSol >> insertN txt >> leftN (length txt)
      _      -> insertN txt' >> leftB

-- | Switching to another mode from visual mode.
--
-- All visual commands are meta actions, as they transfer control to another
-- KeymapM. In this way vis_single is analogous to cmd2other
--
vis_single :: SelectionStyle -> VimMode
vis_single selectionStyle =
        let beginIns a = write (a >> withBuffer0 (setVisibleSelection False)) >> ins_mode
        in choice [
            spec KEsc ?>> return (),
            char 'V'  ?>> change_vis_mode selectionStyle (SelectionStyle Line),
            char 'v'  ?>> change_vis_mode selectionStyle (SelectionStyle Character),
            char ':'  ?>> ex_mode ":'<,'>",
            char 'y'  ?>>! yankSelection,
            char 'x'  ?>>! cutSelection,
            char 'd'  ?>>! cutSelection,
            char 'p'  ?>>! pasteOverSelection,
            char 'c'  ?>> beginIns cutSelection]


-- | These also switch mode, as all visual commands do, but these are
-- analogous to the commands in cmd_eval.  They are different in that
-- they are multiple characters
vis_multi :: VimMode
vis_multi = do
   cnt <- count
   let i = maybe 1 id cnt
   choice ([pString "ZZ" >>! (viWrite >> quitEditor),
            char '>' ?>>! shiftIndentOfSelection i,
            char '<' ?>>! shiftIndentOfSelection (-i),
            char 'r' ?>> do x <- textChar
                            write $ do
                                   mrk <- getSelectionMarkPointB
                                   pt <- pointB
                                   text <- readRegionB (mkVimRegion mrk pt)
                                   moveTo mrk
                                   deleteRegionB (mkVimRegion mrk pt)
                                   let convert '\n' = '\n'
                                       convert  _   = x
                                   insertN $ map convert $ text] ++
           [c ?>>! action i | (c,action) <- singleCmdFM ])


-- | Switch to another vim mode from command mode.
--
-- These commands are meta actions, as they transfer control to another
-- KeymapM. Some of these commands also perform an action before switching.
--
cmd2other :: VimMode
cmd2other = let beginIns a = write a >> ins_mode
                beginIns :: EditorM () -> VimMode
        in choice [
            char ':'     ?>> ex_mode ":",
            char 'v'     ?>> vis_mode (SelectionStyle Character),
            char 'V'     ?>> vis_mode (SelectionStyle Line),
            char 'R'     ?>> rep_mode,
            char 'i'     ?>> ins_mode,
            char 'I'     ?>> beginIns (withBuffer0 moveToSol),
            char 'a'     ?>> beginIns $ withBuffer0 $ moveXorEol 1,
            char 'A'     ?>> beginIns (withBuffer0 moveToEol),
            char 'o'     ?>> beginIns $ withBuffer0 $ moveToEol >> insertB '\n',
            char 'O'     ?>> beginIns $ withBuffer0 $ moveToSol >> insertB '\n' >> lineUp,
            char 'c'     ?>> do (regionStyle, m) <- gen_cmd_move ; beginIns $ cut m regionStyle,
            pString "cc"  >> beginIns (withBuffer0 moveToSol >> cut viMoveToEol LineWise),
            char 'C'     ?>> beginIns $ cut viMoveToEol Exclusive, -- alias of "c$"
            char 'S'     ?>> beginIns $ withBuffer0 moveToSol >> cut viMoveToEol Exclusive, -- non-linewise alias of "cc"
            char 's'     ?>> beginIns $ cut (CharMove Forward) Exclusive, -- non-linewise alias of "cl"
            char '/'     ?>> ex_mode "/",
            char '?'     ?>>! not_implemented '?',
            leave,
            spec KIns    ?>> ins_mode]


ins_rep_char :: VimMode
ins_rep_char = choice [spec KPageUp   ?>>! upScreenB,
                       spec KPageDown ?>>! downScreenB,
                       spec KUp       ?>>! lineUp,
                       spec KDown     ?>>! lineDown,
                       spec KLeft     ?>>! moveXorSol 1,
                       spec KRight    ?>>! moveXorEol 1,
                       spec KEnd      ?>>! moveToEol,
                       spec KHome     ?>>! moveToSol,
                       spec KDel      ?>>! deleteB Character Forward,
                       spec KEnter    ?>>! insertB '\n',
                       spec KTab      ?>>! insertTabB,
                       (ctrl $ char 'w') ?>>! cut (GenMove ViWord (Backward,InsideBound) Backward) Exclusive]


-- ---------------------------------------------------------------------
-- | vim insert mode
--
-- Some ideas for a better insert mode are contained in:
--
--      Poller and Garter , "A comparative study of moded and modeless
--      text editing by experienced editor users", 1983
--
-- which suggest that movement commands be added to insert mode, along
-- with delete.
--
ins_char :: VimMode
ins_char = (spec KBS ?>>! deleteB Character Backward)
           <|> ins_rep_char
           <|| (textChar >>= write . insertB)

-- --------------------
-- | Keyword
kwd_mode :: VimMode
kwd_mode = some ((ctrl $ char 'n') ?>> adjustPriority (-1) (write wordCompleteB)) >> (write resetCompleteB)
           -- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
           -- and resetting it (restarting at the 1st completion).

-- ---------------------------------------------------------------------
-- | vim replace mode
--
-- To quote vim:
--  In Replace mode, one character in the line is deleted for every character
--  you type.  If there is no character to delete (at the end of the line), the
--  typed character is appended (as in Insert mode).  Thus the number of
--  characters in a line stays the same until you get to the end of the line.
--  If a <NL> is typed, a line break is inserted and no character is deleted.
rep_char :: VimMode
rep_char = (spec KBS ?>>! leftB) -- should undo unless pointer has been moved
           <|> ins_rep_char
           <|| do c <- textChar; write (do e <- atEol; if e then insertB c else writeB c)

-- ---------------------------------------------------------------------
-- Ex mode. We also process regex searching mode here.

spawn_ex_buffer :: String -> YiM ()
spawn_ex_buffer prompt = do
  -- The above ensures that the action is performed on the buffer that originated the minibuffer.
  let ex_buffer_finish = do
        withEditor $ historyFinish
        lineString <- withBuffer elemsB
        withEditor closeBufferAndWindowE
        ex_eval (head prompt : lineString)
      ex_process :: VimMode
      ex_process =
          choice [spec KEnter ?>>! ex_buffer_finish,
                  spec KTab   ?>>! completeMinibuffer,
                  spec KEsc   ?>>! closeBufferAndWindowE,
                  spec KBS    ?>>! deleteB Character Backward,
                  spec KDel   ?>>! deleteB Character Forward,
                  spec KUp    ?>>! historyUp,
                  spec KDown  ?>>! historyDown,
                  spec KLeft  ?>>! moveXorSol 1,
                  spec KRight ?>>! moveXorEol 1]
             <|| (textChar >>= write . insertB)
      completeMinibuffer = withBuffer elemsB >>= ex_complete >>= withBuffer . insertN
      b_complete f = completeBufferName f >>= return . drop (length f)
      ex_complete ('e':' ':f) = completeFileName Nothing f >>= return . drop (length f)
      ex_complete ('b':' ':f)                             = b_complete f
      ex_complete ('b':'u':'f':'f':'e':'r':' ':f)         = b_complete f
      ex_complete ('b':'d':' ':f)                         = b_complete f
      ex_complete ('b':'d':'!':' ':f)                     = b_complete f
      ex_complete ('b':'d':'e':'l':'e':'t':'e':' ':f)     = b_complete f
      ex_complete ('b':'d':'e':'l':'e':'t':'e':'!':' ':f) = b_complete f
      ex_complete _ = return ""

  withEditor $ historyStart
  spawnMinibufferE prompt (const $ ex_process) 
  return ()


ex_mode :: String -> VimMode
ex_mode = write . spawn_ex_buffer

-- | eval an ex command to an YiM (), also appends to the ex history
ex_eval :: String -> YiM ()
ex_eval cmd = do
  case cmd of
        -- regex searching
          ('/':pat) -> doSearch (Just pat) [] Forward

        -- TODO: We give up on re-mapping till there exists a generic Yi mechanism to do so.

        -- add mapping to command mode
          (_:'m':'a':'p':' ':_cs) -> error "Not yet implemented."

        -- add mapping to insert mode
          (_:'m':'a':'p':'!':' ':_cs) -> error "Not yet implemented."

        -- unmap a binding from command mode
          (_:'u':'n':'m':'a':'p':' ':_cs) -> error "Not yet implemented."

        -- unmap a binding from insert mode
          (_:'u':'n':'m':'a':'p':'!':' ':_cs) -> error "Not yet implemented."


        -- just a normal ex command
          (_:src) -> fn src

        -- can't happen, but deal with it
          [] -> return ()

    where
      isWorthlessB = gets (\b -> isUnchangedBuffer b || file b == Nothing)
      whenUnchanged mu f = do u <- mu
                              if u then f
                                   else errorEditor "No write since last change (add ! to override)"
      quitB = whenUnchanged (withBuffer isWorthlessB) closeWindow

      quitNoW = do bufs <- withEditor $ do closeBufferE ""
                                           bufs <- gets bufferStack
                                           mapM (\x -> withGivenBuffer0 x isWorthlessB) bufs
                   whenUnchanged (return $ all id bufs) quitEditor

      quitall  = withAllBuffers quitB
      wquitall = withAllBuffers viWrite >> quitEditor
      bdelete  = whenUnchanged (withBuffer isUnchangedB) . withEditor . closeBufferE
      bdeleteNoW = withEditor . closeBufferE

      fn ""           = withEditor msgClr

      fn s@(c:_) | isDigit c = do
        e <- liftIO $ try $ evaluate $ read s
        case e of Left _ -> errorEditor $ "The " ++show s++ " command is unknown."
                  Right lineNum -> withBuffer (gotoLn lineNum) >> return ()

      fn "w"          = viWrite
      fn ('w':' ':f)  = viWriteTo $ strip f
      fn "qa"         = quitall
      fn "qal"        = quitall
      fn "qall"       = quitall
      fn "quita"      = quitall
      fn "quital"     = quitall
      fn "quitall"    = quitall
      fn "q"          = quitB
      fn "qu"         = quitB
      fn "qui"        = quitB
      fn "quit"       = quitB
      fn "q!"         = quitNoW
      fn "qu!"        = quitNoW
      fn "qui!"       = quitNoW
      fn "quit!"      = quitNoW
      fn "qa!"        = quitEditor
      fn "quita!"     = quitEditor
      fn "quital!"    = quitEditor
      fn "quitall!"   = quitEditor
      fn "wq"         = viWrite >> closeWindow
      fn "wqa"        = wquitall
      fn "wqal"       = wquitall
      fn "wqall"      = wquitall
      fn "x"          = do unchanged <- withBuffer isUnchangedB
                           unless unchanged viWrite
                           closeWindow
      fn "n"          = withEditor nextBufW
      fn "next"       = withEditor nextBufW
      fn "$"          = withBuffer botB
      fn "p"          = withEditor prevBufW
      fn "prev"       = withEditor prevBufW
      fn ('s':'p':_)  = withEditor splitE
      fn "e"          = revertE
      fn ('e':' ':f)  = fnewE f
      -- fn ('s':'e':'t':' ':'f':'t':'=':ft)  = withBuffer $ setSyntaxB $ highlighters M.! ft
      fn ('n':'e':'w':' ':f) = withEditor splitE >> fnewE f
      fn ('s':'/':cs) = viSub cs

      fn ('b':' ':"m") = withEditor $ switchToBufferWithNameE "*messages*"
      fn ('b':' ':f)   = withEditor $ switchToBufferWithNameE f
      fn "bd"                                    = bdelete ""
      fn "bdelete"                               = bdelete ""
      fn ('b':'d':' ':f)                         = bdelete f
      fn ('b':'d':'e':'l':'e':'t':'e':' ':f)     = bdelete f
      fn "bd!"                                   = bdeleteNoW ""
      fn "bdelete!"                              = bdeleteNoW ""
      fn ('b':'d':'!':' ':f)                     = bdeleteNoW f
      fn ('b':'d':'e':'l':'e':'t':'e':'!':' ':f) = bdeleteNoW f
      -- TODO: bd[!] [N]

      -- send just this line through external command /fn/
      fn ('.':'!':f) = do
            ln  <- withBuffer readLnB
            ln' <- runProcessWithInput f ln
            withBuffer $ do moveToSol
                            deleteToEol
                            insertN ln'
                            moveToSol

--    Needs to occur in another buffer
--    fn ('!':f) = runProcessWithInput f []

      fn "reload"     = reloadEditor >> return ()    -- not in vim
      -- fn "eval"       = withBuffer (regionOfB Document >>= readRegionB) >>= evalE -- not in vim

      fn "redr"       = refreshEditor
      fn "redraw"     = refreshEditor

      fn "u"          = withBuffer undoB
      fn "undo"       = withBuffer undoB
      fn "red"        = withBuffer redoB
      fn "redo"       = withBuffer redoB

      fn "sus"        = suspendEditor
      fn "suspend"    = suspendEditor
      fn "st"         = suspendEditor
      fn "stop"       = suspendEditor

      fn s            = errorEditor $ "The "++show s++ " command is unknown."


------------------------------------------------------------------------

not_implemented :: Char -> YiM ()
not_implemented c = errorEditor $ "Not implemented: " ++ show c

-- ---------------------------------------------------------------------
-- Misc functions

withAllBuffers :: YiM () -> YiM ()
withAllBuffers m = mapM_ (\b -> withEditor (setBuffer b) >> m) =<< readEditor bufferStack

viFileInfo :: YiM ()
viFileInfo =
    do bufInfo <- withBuffer bufInfoB
       msgEditor $ showBufInfo bufInfo
    where
    showBufInfo :: BufferFileInfo -> String
    showBufInfo bufInfo = concat [ show $ bufInfoFileName bufInfo
         , " Line "
         , show $ bufInfoLineNo bufInfo
         , " ["
         , bufInfoPercent bufInfo
         , "]"
         ]


-- | Try to write a file in the manner of vi\/vim
-- Need to catch any exception to avoid losing bindings
viWrite :: YiM ()
viWrite = do
    mf <- withBuffer getfileB
    case mf of
        Nothing -> errorEditor "no file name associate with buffer"
        Just f  -> viWriteTo f

-- | Try to write to a named file in the manner of vi\/vim
viWriteTo :: String -> YiM ()
viWriteTo f = do
    bufInfo <- withBuffer bufInfoB
    let s   = bufInfoFileName bufInfo
    let msg = msgEditor $ show f++" "++show s ++ " written"
    catchJustE ioErrors (fwriteToE f >> msg) (msgEditor . show)

strip :: String -> String
strip = takeWhile (/= ' ') . dropWhile (== ' ')

-- | Try to do a substitution
viSub :: [Char] -> YiM ()
viSub cs = do
    let (pat,rep') = break (== '/')  cs
        (rep,opts) = case rep' of
                        []     -> ([],[])
                        (_:ds) -> case break (== '/') ds of
                                    (rep'', [])    -> (rep'', [])
                                    (rep'', (_:fs)) -> (rep'',fs)
    case opts of
        []    -> do_single pat rep
        ['g'] -> do_single pat rep
        _     -> do_single pat rep-- TODO

    where do_single p r = do
                s <- searchAndRepLocal p r
                if not s then errorEditor ("Pattern not found: "++p) else withEditor msgClr

