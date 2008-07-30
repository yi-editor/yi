{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008 Nicolas Pouillard


-- | Vim keymap for Yi. Emulates vim :set nocompatible
module Yi.Keymap.Vim (keymap, 
                      viWrite, 
                      defKeymap, 
                      ModeMap(..),
                      mkKeymap) where

import Yi.Prelude
import Prelude (maybe, length, filter, map, drop, break, uncurry)

import Data.Char
import Data.List (sort, nub)
import Data.Dynamic

import Control.Exception    ( ioErrors, try, evaluate )
import Control.Monad.State hiding (mapM_, mapM)
import Control.Applicative

import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Buffer.Indent
import Yi.Core
import Yi.Dired
import Yi.Editor
import Yi.Eval (execEditorAction)
import Yi.File
import Yi.History
import Yi.Misc (matchingFileNames)
import Yi.String (dropSpace)
import Yi.Keymap.Keys
import Yi.MiniBuffer
import Yi.Search
import Yi.TextCompletion

--
-- What's missing?
--   fancier :s//
--   '.'
--   movement parameterised \> \<
--

-- ---------------------------------------------------------------------

type VimMode = Keymap

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


mkKeymap :: (ModeMap -> ModeMap) -> VimMode
mkKeymap = v_top_level . fix

keymap :: VimMode
keymap = mkKeymap defKeymap

-- | The Vim keymap is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate VimProcs for each phase of key input in that mode.

data ModeMap = ModeMap {
      -- | Top level mode
      v_top_level :: VimMode,

      -- | vim insert mode
      v_ins_char :: VimMode
    }

defKeymap :: ModeMap -> ModeMap
defKeymap self = ModeMap {
                   v_top_level = def_top_level,
                   v_ins_char = def_ins_char
                 }
     where
     -- | Top level consists of simple commands that take a count arg,
     -- the replace cmd, which consumes one char of input, and commands
     -- that switch modes.
     def_top_level = choice [cmd_eval,cmd_move,cmd2other,cmd_op]

     -- | Leave a mode. This always has priority over catch-all actions inside the mode.
     leave :: VimMode
     leave = oneOf [spec KEsc, ctrl $ char 'c'] >> adjustPriority (-1) (write msgClr)

     -- | Insert mode is either insertion actions, or the meta (\ESC) action
     ins_mode :: VimMode
     ins_mode = write (setStatus "-- INSERT --") >> many (ins_char <|> kwd_mode) >> leave

     beginIns :: (Show x, YiAction a x) => a -> I Event Action ()
     beginIns a = write a >> ins_mode

     -- | Replace mode is like insert, except it performs writes, not inserts
     rep_mode :: VimMode
     rep_mode = write (setStatus "-- REPLACE --") >> many rep_char >> leave

     -- | Reset the selection style to a character-wise mode 'SelectionStyle Character'.
     resetSelectStyle :: BufferM ()
     resetSelectStyle = setDynamicB $ SelectionStyle Character

     -- | Visual mode, similar to command mode
     vis_mode :: SelectionStyle -> VimMode
     vis_mode selectionStyle = do
       write (setVisibleSelection True >> pointB >>= setSelectionMarkPointB)
       core_vis_mode selectionStyle
       write (msgClr >> withBuffer0 (setVisibleSelection False) >> withBuffer0 resetSelectStyle)

     core_vis_mode :: SelectionStyle -> VimMode
     core_vis_mode selectionStyle = do
       write $ do withBuffer0 $ setDynamicB $ selectionStyle
                  setStatus $ msg selectionStyle
       many (cmd_move <|>
             select_any_unit (withBuffer0 . (\r -> resetSelectStyle >> extendSelectRegionB r >> leftB)))
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

     cmd_move :: VimMode
     cmd_move = gen_cmd_move >>= write . viMove . snd

     -- the returned RegionStyle is used when the movement is combined with a 'cut' or 'yank'.
     gen_cmd_move :: KeymapM (RegionStyle, ViMove)
     gen_cmd_move = do
       cnt <- count
       let x = maybe 1 id cnt
       choice ([c ?>> return (Inclusive, a x) | (c,a) <- moveCmdFM_inclusive, (c /= char '0' || Nothing == cnt) ] ++
               [pString s >> return (Inclusive, a x) | (s,a) <- moveCmdS_inclusive ] ++
               [c ?>> return (Exclusive, a x) | (c,a) <- moveCmdFM_exclusive ] ++
               [pString s >> return (Exclusive, a x) | (s,a) <- moveCmdS_exclusive ] ++
               [c ?>> return (LineWise, a x) | (c,a) <- moveUpDownCmdFM] ++
               [do event c; c' <- textChar; return (r, a c' x) | (c,r,a) <- move2CmdFM] ++
               [char 'G' ?>> return (LineWise, ArbMove (case cnt of
                                                          Nothing -> botB >> moveToSol
                                                          Just n  -> gotoLn n >> return ()))
               ,pString "gg" >> return (LineWise, ArbMove (gotoLn 0 >> return ()))])

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
         ,(char 'W',       Replicate $ GenMove ViWORD (Backward,InsideBound) Forward)
         ,(char 'b',       Replicate $ Move ViWord Backward)
         ,(char 'B',       Replicate $ Move ViWORD Backward)
          -- text
         ,(char '{',       Replicate $ Move unitEmacsParagraph Backward)
         ,(char '}',       Replicate $ Move unitEmacsParagraph Forward)
         ,(char '(',       Replicate $ Move unitSentence  Backward)
         ,(char ')',       Replicate $ Move unitSentence  Forward)
         ]
         where
             left  = Replicate $ CharMove Backward
             right = Replicate $ CharMove Forward
             sol   = Replicate $ viMoveToSol

     -- | movement *multi-chars* commands (with exclusive cut/yank semantics)
     moveCmdS_exclusive :: [(String, (Int -> ViMove))]
     moveCmdS_exclusive =
         [("[(", Replicate $ ArbMove (goUnmatchedB Backward '(' ')'))
         ,("[{", Replicate $ ArbMove (goUnmatchedB Backward '{' '}'))
         ,("])", Replicate $ ArbMove (goUnmatchedB Forward  '(' ')'))
         ,("]}", Replicate $ ArbMove (goUnmatchedB Forward  '{' '}'))
         ]

     -- | movement commands (with inclusive cut/yank semantics)
     moveCmdFM_inclusive :: [(Event, (Int -> ViMove))]
     moveCmdFM_inclusive =
     -- left/right
         [(char '$',  eol)
         ,(spec KEnd, eol)
     -- words
         ,(char 'e',     Replicate $ GenMove ViWord (Forward, InsideBound) Forward)
         ,(char 'E',     Replicate $ GenMove ViWORD (Forward, InsideBound) Forward)]
         where
             eol   = Replicate $ viMoveToEol

     -- | movement *multi-chars* commands (with inclusive cut/yank semantics)
     moveCmdS_inclusive :: [(String, (Int -> ViMove))]
     moveCmdS_inclusive =
         [("ge", Replicate $ GenMove ViWord (Forward, InsideBound) Backward)
         ,("gE", Replicate $ GenMove ViWORD (Forward, InsideBound) Backward)
         ,("g_", const $ ArbMove lastNonSpaceB)]

     regionOfViMove :: ViMove -> RegionStyle -> BufferM Region
     regionOfViMove move regionStyle =
       join $ regionFromTo <$> pointB
                           <*> (savingPointB (viMove move >> pointB))
                           <*> pure regionStyle

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
     viReplicateMove (Replicate move j)    i = viReplicateMove move (i * j)
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
          [char 'r' ?>> textChar >>= write . writeN . replicate i])

     -- TODO: escape the current word
     --       at word bounds: search for \<word\>
     searchCurrentWord :: Direction -> EditorM ()
     searchCurrentWord dir = do
       w <- withBuffer0 $ readRegionB =<< regionOfB ViWord
       doSearch (Just w) [] dir

     -- | Parse any character that can be inserted in the text.
     textChar :: KeymapM Char
     textChar = do
       Event (KASCII c) [] <- anyEvent
       return c

     continueSearching :: (Direction -> Direction) -> EditorM ()
     continueSearching fdir = do
       m <- getRegexE
       dir <- fdir <$> getA searchDirectionA 
       let s = case m of
                       Nothing -> ""
                       Just (s',_) -> s'
       printMsg $ case dir of { Forward -> '/':s ; Backward -> '?':s }
       doSearch Nothing [] dir

     -- | cmd mode commands
     -- An event specified paired with an action that may take an integer argument.
     -- Usually the integer argument is the number of times an action should be repeated.
     singleCmdFM :: [(Event, Int -> YiM ())]
     singleCmdFM =
         [(ctrl $ char 'b',    withBuffer . upScreensB)             -- vim does (firstNonSpaceB;moveXorSol)
         ,(ctrl $ char 'f',    withBuffer . downScreensB)
         ,(ctrl $ char 'g',    const viFileInfo)
         ,(ctrl $ char 'l',    const refreshEditor)
         ,(ctrl $ char 'r',    withBuffer . flip replicateM_ redoB)
         ,(ctrl $ char 'z',    const suspendEditor)
         ,(char 'D',      withEditor . cut Exclusive . (Replicate $ Move Line Forward))
         ,(char 'J',      const (withBuffer (moveToEol >> deleteN 1)))    -- the "\n"
         ,(char 'Y',      \n -> withEditor $ do
                                    let move = (Replicate $ Move Line Forward) n
                                    region <- withBuffer0 $ regionOfViMove move Inclusive
                                    yankRegion LineWise region
          )
         ,(char 'U',      withBuffer . flip replicateM_ undoB)    -- NB not correct
         ,(char 'n',      const $ withEditor $ continueSearching id)
         ,(char 'N',      const $ withEditor $ continueSearching reverseDir)
         ,(char 'u',      withBuffer . flip replicateM_ undoB)

         ,(char 'X',      withEditor . cut Exclusive . (Replicate $ Move Character Backward))
         ,(char 'x',      withEditor . cut Exclusive . (Replicate $ Move Character Forward))

         ,(char 'p',      withEditor . flip replicateM_ pasteAfter)

         ,(char 'P',      withEditor . flip replicateM_ pasteBefore)

         ,(spec KPageUp, withBuffer . upScreensB)
         ,(spec KPageDown, withBuffer . downScreensB)
         ,(char '*',      const $ withEditor $ searchCurrentWord Forward)
         ,(char '#',      const $ withEditor $ searchCurrentWord Backward)
         ,(char '~',      \i -> withBuffer $ do
                              p <- pointB
                              moveXorEol i
                              q <- pointB
                              moveTo p
                              mapRegionB (mkRegion p q) switchCaseChar
                              moveTo q)
         -- The count value , in this case, is interpretted as a percentage instead of a repeat
         -- count.
         ,(char '%',      \i -> withBuffer $ do
                              let f :: Double
                                  f  = case (fromIntegral i / 100.0) of
                                          x | x > 1.0 -> 1.0
                                            | x < 0.0 -> 0.0 -- Impossible?
                                            | otherwise -> x
                              Point max_p <- sizeB 
                              let p = floor (fromIntegral max_p * f)
                              moveTo $ Point p
                              moveToSol
          )
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
       choice $ [let onMove regionStyle move =
                        onRegion regionStyle =<< withBuffer0 (regionOfViMove move regionStyle)
                     s1 = prefix [c]
                     ss = nub [[c], s1]
                 in
                 pString s1 >>
                    choice ([ gen_cmd_move >>= (\(regionStyle, m) -> write $ onMove regionStyle (Replicate m i))
                            , select_any_unit (onRegion Exclusive)] ++
                            [ pString s >>! onMove LineWise (Replicate (Move VLine Forward) (i-1))
                            | s <- ss ])

                | (prefix,c,onRegion) <- opCmdFM
                ]
         where
             -- | operator (i.e. movement-parameterised) actions
             opCmdFM =  [ (id,     'd', cutRegion)
                        , (id,     'y', yankRegion)
                        , (('g':), '~', viMapRegion switchCaseChar)
                        , (('g':), 'u', viMapRegion toLower)
                        , (('g':), 'U', viMapRegion toUpper)
                        , (('g':), '?', viMapRegion rot13Char)
                        , (('g':), 'q', const $ withBuffer0 . fillRegion)
                        , (('g':), 'w', const $ withBuffer0 . savingPointB . fillRegion)
                        ]

     char2unit :: [(Char, TextUnit)]
     char2unit =
       [('w',  ViWord)
       ,('W',  ViWORD)
       ,('p',  unitEmacsParagraph)
       ,('s',  unitSentence)
       ,('"',  Delimited '"' '"')
       ,('`',  Delimited '`' '`')
       ,('\'', Delimited '\'' '\'')
       ,('(',  Delimited '(' ')')
       ,(')',  Delimited '(' ')')
       ,('b',  Delimited '(' ')')
       ,('{',  Delimited '{' '}')
       ,('}',  Delimited '{' '}')
       ,('B',  Delimited '{' '}')
       ,('<',  Delimited '<' '>')
       ,('>',  Delimited '<' '>')
       ]

     -- NOTE,TODO: Form some units (like words) one should
     -- not select more than the current line
     select_a_unit :: TextUnit -> BufferM Region
     select_a_unit (Delimited cStart cStop) = savingPointB $ do
         start <- untilB ((==cStart) <$> readB) leftB >> pointB
         rightB
         stop  <- untilB ((==cStop) <$> readB) rightB >> rightB >> pointB
         return $ mkRegion start stop
     select_a_unit unit = savingPointB $ do
         start <- genMaybeMoveB unit (Backward,InsideBound) Backward >> pointB
         stop  <- genMoveB unit (Backward,InsideBound) Forward >> pointB
         return $ mkRegion start stop

     select_inner_unit :: TextUnit -> BufferM Region
     select_inner_unit = regionOfB

     select_any_unit :: (MonadInteract m Action Event) => (Region -> EditorM ()) -> m ()
     select_any_unit f =
       choice [ x
              | (c, unit) <- char2unit,
                x <- [ char 'i' ?>> (char c ?>> write (f =<< withBuffer0 (select_inner_unit unit))),
                       char 'a' ?>> (char c ?>> write (f =<< withBuffer0 (select_a_unit unit))) ] ]

     regionFromTo :: Point -> Point -> RegionStyle -> BufferM Region
     regionFromTo start' stop' regionStyle =
       let [start, stop] = sort [start', stop']
           region = mkRegion start stop in
       case regionStyle of
         LineWise  -> inclusiveRegionB =<< unitWiseRegion Line region
         Inclusive -> inclusiveRegionB region
         Exclusive -> return region

     regionOfSelection :: BufferM (RegionStyle, Region)
     regionOfSelection = do
       regionStyle <- selection2regionStyle <$> getDynamicB
       region <- join $ regionFromTo <$> getSelectionMarkPointB
                                     <*> pointB
                                     <*> pure regionStyle
       return (regionStyle, region)

     yankRegion :: RegionStyle -> Region -> EditorM ()
     yankRegion regionStyle region = do
       txt <- withBuffer0 $ readRegionB region
       setRegE $ if (regionStyle == LineWise) then '\n':txt else txt
       let rowsYanked = length (filter (== '\n') txt)
       when (rowsYanked > 2) $ printMsg $ show rowsYanked ++ " lines yanked"

     {-
     yank :: RegionStyle -> ViMove -> EditorM ()
     yank regionStyle move =
       yankRegion regionStyle =<< (withBuffer0 $ regionOfViMove move regionStyle)
     -}

     yankSelection :: EditorM ()
     yankSelection = uncurry yankRegion =<< withBuffer0 regionOfSelection

     cutRegion :: RegionStyle -> Region -> EditorM ()
     cutRegion regionStyle region = do
       txt <- withBuffer0 $ do
         txt <- readRegionB region
         deleteRegionB region
         return txt
       setRegE $ if (regionStyle == LineWise) then '\n':txt else txt
       let rowsCut = length (filter (== '\n') txt)
       when (rowsCut > 2) $ printMsg $ show rowsCut ++ " fewer lines"

     cut :: RegionStyle -> ViMove -> EditorM ()
     cut regionStyle move =
       cutRegion regionStyle =<< (withBuffer0 $ regionOfViMove move regionStyle)

     cutSelection :: EditorM ()
     cutSelection = uncurry cutRegion =<< withBuffer0 regionOfSelection

     pasteOverSelection :: EditorM ()
     pasteOverSelection = do
       txt <- getRegE
       withBuffer0 $ do
         selectionStyle <- getDynamicB
         start <- getSelectionMarkPointB
         stop <- pointB
         region <- regionFromTo start stop $ selection2regionStyle $ selectionStyle
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

     switchCaseChar :: Char -> Char
     switchCaseChar c = if isUpper c then toLower c else toUpper c

     onCharLetterCode :: (Int -> Int) -> Char -> Char
     onCharLetterCode f c | isUpper c || isLower c = chr (f (ord c - a) `mod` 26 + a)
                          | otherwise              = c
                         where a | isUpper c = ord 'A'
                                 | isLower c = ord 'a'
                                 | otherwise = undefined

     rot13Char :: Char -> Char
     rot13Char = onCharLetterCode (+13)

     viMapRegion :: (Char -> Char) -> RegionStyle -> Region -> EditorM ()
     viMapRegion f _ region = withBuffer0 $ mapRegionB region f

     -- | Switching to another mode from visual mode.
     --
     -- All visual commands are meta actions, as they transfer control to another
     -- KeymapM. In this way vis_single is analogous to cmd2other
     --
     vis_single :: SelectionStyle -> VimMode
     vis_single selectionStyle =
         choice [spec KEsc ?>> return (),
                 char 'V'  ?>> change_vis_mode selectionStyle (SelectionStyle Line),
                 char 'v'  ?>> change_vis_mode selectionStyle (SelectionStyle Character),
                 char ':'  ?>>! ex_mode ":'<,'>",
                 char 'y'  ?>>! yankSelection,
                 char 'x'  ?>>! cutSelection,
                 char 'd'  ?>>! cutSelection,
                 char 'p'  ?>>! pasteOverSelection,
                 char 'c'  ?>> beginIns (cutSelection >> withBuffer0 (setVisibleSelection False))]


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
                                 -- TODO: rewrite in functional style. (modifyRegionB?)
                                 write $ do
                                        mrk <- getSelectionMarkPointB
                                        pt <- pointB
                                        r <- inclusiveRegionB $ mkRegion mrk pt
                                        text <- readRegionB r
                                        moveTo mrk
                                        deleteRegionB r
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
     cmd2other =
         choice [char ':'     ?>>! ex_mode ":",
                 char 'v'     ?>> vis_mode (SelectionStyle Character),
                 char 'V'     ?>> vis_mode (SelectionStyle Line),
                 char 'R'     ?>> rep_mode,
                 char 'i'     ?>> ins_mode,
                 char 'I'     ?>> beginIns moveToSol,
                 char 'a'     ?>> beginIns $ moveXorEol 1,
                 char 'A'     ?>> beginIns moveToEol,
                 char 'o'     ?>> beginIns $ moveToEol >> insertB '\n',
                 char 'O'     ?>> beginIns $ moveToSol >> insertB '\n' >> lineUp,
                 char 'c'     ?>> changeCmds,
                 char 'C'     ?>> beginIns $ cut Exclusive viMoveToEol, -- alias of "c$"
                 char 'S'     ?>> beginIns $ withBuffer0 moveToSol >> cut Exclusive viMoveToEol, -- non-linewise alias of "cc"
                 char 's'     ?>> beginIns $ cut Exclusive (CharMove Forward), -- non-linewise alias of "cl"
                 char '/'     ?>>! ex_mode "/",
                 char '?'     ?>>! ex_mode "?",
                 leave,
                 spec KIns    ?>> ins_mode]

     changeCmds :: I Event Action ()
     changeCmds =
       adjustPriority (-1)
         ((char 'w' ?>> change NoMove Exclusive (GenMove ViWord (Forward, OutsideBound) Forward)) <|>
          (char 'W' ?>> change NoMove Exclusive (GenMove ViWORD (Forward, OutsideBound) Forward))) <|>
       (char 'c' ?>> change viMoveToSol LineWise viMoveToEol) <|>
       (uncurry (change NoMove) =<< gen_cmd_move) <|>
       (select_any_unit (cutRegion Exclusive) >> ins_mode) -- this correct while the RegionStyle is not LineWise

     change :: ViMove -> RegionStyle -> ViMove -> I Event Action ()
     change preMove regionStyle move = do
       write $ do
         withBuffer0 $ viMove preMove
         cut regionStyle move
         when (regionStyle == LineWise) $ withBuffer0 (insertB '\n' >> leftB)
       ins_mode

     ins_rep_char :: VimMode
     ins_rep_char = choice [spec KPageUp       ?>>! upScreenB,
                            spec KPageDown     ?>>! downScreenB,
                            spec KUp           ?>>! lineUp,
                            spec KDown         ?>>! lineDown,
                            spec KLeft         ?>>! moveXorSol 1,
                            spec KRight        ?>>! moveXorEol 1,
                            spec KEnd          ?>>! moveToEol,
                            spec KHome         ?>>! moveToSol,
                            spec KDel          ?>>! deleteB Character Forward,
                            spec KEnter        ?>>! insertB '\n',
                            spec KTab          ?>>! insertTabB,
                            (ctrl $ char 'w')  
                                ?>>! cut Exclusive (GenMove ViWord (Forward,OutsideBound) Backward)]

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
     ins_char = v_ins_char self
     def_ins_char = (spec KBS ?>>! deleteB Character Backward)
                <|> ins_rep_char
                <|| (textChar >>= write . insertB)

     -- --------------------
     -- | Keyword
     kwd_mode :: VimMode
     kwd_mode = some ((ctrl $ char 'n') ?>> adjustPriority (-1) (write wordComplete)) >> (write resetComplete)
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

     ex_mode :: String -> EditorM ()
     ex_mode prompt = do
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
           exSimpleComplete compl s = drop (length s) <$> simpleComplete compl s
           b_complete = exSimpleComplete matchingBufferNames
           ex_complete ('e':' ':f)                             = exSimpleComplete (matchingFileNames Nothing) f
           ex_complete ('b':' ':f)                             = b_complete f
           ex_complete ('b':'u':'f':'f':'e':'r':' ':f)         = b_complete f
           ex_complete ('b':'d':' ':f)                         = b_complete f
           ex_complete ('b':'d':'!':' ':f)                     = b_complete f
           ex_complete ('b':'d':'e':'l':'e':'t':'e':' ':f)     = b_complete f
           ex_complete ('b':'d':'e':'l':'e':'t':'e':'!':' ':f) = b_complete f
           ex_complete ('y':'i':' ':s)                         = exSimpleComplete (\_->getAllNamesInScope) s
           ex_complete _                                       = return ""

       historyStart
       spawnMinibufferE prompt (const $ ex_process)
       return ()

     -- | eval an ex command to an YiM (), also appends to the ex history
     ex_eval :: String -> YiM ()
     ex_eval cmd = do
       case cmd of
             -- regex searching
               ('/':pat) -> withEditor $ doSearch (Just pat) [] Forward
               ('?':pat) -> withEditor $ doSearch (Just pat) [] Backward

             -- TODO: Remapping could be done using the <|| operator somehow. 
             -- The remapped stuff could be saved in a keymap-local state, (using StateT monad transformer).

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
           fn ('w':' ':f)  = viWriteTo $ dropSpace f
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
           fn ('s':'/':cs) = withEditor $ viSub cs

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

           fn ('y':'i':' ':s) = execEditorAction s
           fn s            = errorEditor $ "The "++show s++ " command is unknown."


     ------------------------------------------------------------------------

     --not_implemented :: Char -> YiM ()
     --not_implemented c = errorEditor $ "Not implemented: " ++ show c

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
    mf <- withBuffer $ getA fileA
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

-- | Try to do a substitution
viSub :: String -> EditorM ()
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
                if not s then fail ("Pattern not found: "++p) else msgClr

