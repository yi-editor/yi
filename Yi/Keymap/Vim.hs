{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008 Nicolas Pouillard

-- | Vim keymap for Yi. Emulates vim :set nocompatible
module Yi.Keymap.Vim (keymap, 
                      viWrite, 
                      defKeymap, 
                      ModeMap(..),
                      mkKeymap,
                      beginIns) where

import Prelude (maybe, length, filter, map, drop, break, uncurry)

import Data.Char
import Data.List (nub, take, words)
import Data.Prototype
import Numeric (showHex, showOct)
import System.IO (readFile)
import System.Posix.Files (fileExist)

import Control.Monad.State hiding (mapM_, mapM)

import {-# source #-} Yi.Boot
import Yi.Core
import Yi.Dired
import Yi.Eval (execEditorAction)
import Yi.File
import Yi.History
import Yi.Misc (matchingFileNames,adjBlock,adjIndent,cabalRun)
import Yi.String (dropSpace,split)
import Yi.MiniBuffer
import Yi.Search
import Yi.Style
import Yi.TextCompletion
import Yi.Tag (Tag,TagTable,lookupTag,importTagTable)
import Yi.Window (bufkey)


--
-- What's missing?
--   ESC should leave from the visual mode
--   fancier :s// ==> missing /c, ...
--   '.'
--   @:
--   g8
--   8g8
--   :sh[ell]
--   :!!
--   movement parameterised \> \<
--   motion operators [motion.txt]: !, =, >, <
--   C-y in input mode
--   C-v: visual block mode
--   Support for marks
--   C-o and C-i
--

-- ---------------------------------------------------------------------

type VimMode = Keymap

data ViMove = Move TextUnit Direction
            | MaybeMove TextUnit Direction
            | GenMove TextUnit (Direction, BoundarySide) Direction
            | CharMove Direction
            | ArbMove (BufferM ())
            | Replicate ViMove Int
            | SeqMove ViMove ViMove
            | NoMove


mkKeymap :: (Proto ModeMap) -> VimMode
mkKeymap = v_top_level . extractValue

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

defKeymap :: Proto ModeMap
defKeymap = Proto template
  where 
    template self = ModeMap {
                   v_top_level = def_top_level,
                   v_ins_char = def_ins_char
                 }
     where
     -- | Top level consists of simple commands that take a count arg,
     -- the replace cmd, which consumes one char of input, and commands
     -- that switch modes.
     def_top_level = do write clrStatus
                        -- if the keymap "crashed" we restart here
                        -- so we clear the status line to indicate whatever mode we were in
                        -- has been left
                        choice [cmd_eval,cmd_move,cmd2other,cmd_op]

     -- | Replace mode is like insert, except it performs writes, not inserts
     rep_mode :: VimMode
     rep_mode = write (setStatus ("-- REPLACE --", defaultStyle)) >> many rep_char >> leave >> write (moveXorSol 1)

     -- | Reset the selection style to a character-wise mode 'SelectionStyle Character'.
     resetSelectStyle :: BufferM ()
     resetSelectStyle = setDynamicB $ SelectionStyle Character

     -- | Visual mode, similar to command mode
     vis_move :: VimMode
     vis_move = gen_cmd_move >>= write . viMove . snd

     vis_mode :: SelectionStyle -> VimMode
     vis_mode selStyle = do
       write (setVisibleSelection True >> pointB >>= setSelectionMarkPointB)
       core_vis_mode selStyle
       write (clrStatus >> withBuffer0 (setVisibleSelection False) >> withBuffer0 resetSelectStyle)
     
     core_vis_mode :: SelectionStyle -> VimMode
     core_vis_mode selStyle = do
       write $ do withBuffer0 $ setDynamicB $ selStyle
                  setStatus $ (msg selStyle, defaultStyle)
       many (vis_move <|>
             select_any_unit (withBuffer0 . (\r -> resetSelectStyle >> extendSelectRegionB r >> leftB)))
       (vis_single selStyle <|| vis_multi)
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
     count = (deprioritize >> pure Nothing) <|> do
         c <- charOf id '1' '9'
         cs <- many $ charOf id '0' '9'
         return $ Just $ read (c:cs)

     viMoveToNthEol :: Int -> BufferM ()
     viMoveToNthEol n = (replicateM_ n $ moveB Line Forward)

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
     cmd_move = gen_cmd_move >>= write . viMoveNM . snd

     -- the returned RegionStyle is used when the movement is combined with a 'cut' or 'yank'.
     gen_cmd_move :: KeymapM (RegionStyle, ViMove)
     gen_cmd_move = choice
        [ char '0' ?>> return (Exclusive, viMoveToSol)
        , char '%' ?>> return percentMove
        , do 
          cnt <- count
          let x = maybe 1 id cnt
          choice ([c ?>> return (Inclusive, a x) | (c,a) <- moveCmdFM_inclusive ] ++
                  [pString s >> return (Inclusive, a x) | (s,a) <- moveCmdS_inclusive ] ++
                  [c ?>> return (Exclusive, a x) | (c,a) <- moveCmdFM_exclusive ] ++
                  [pString s >> return (Exclusive, a x) | (s,a) <- moveCmdS_exclusive ] ++
                  [c ?>> return (LineWise, a x) | (c,a) <- moveUpDownCmdFM] ++
                  [do event c; c' <- textChar; return (r, a c' x) | (c,r,a) <- move2CmdFM] ++
                  [char 'G' ?>> return (LineWise, ArbMove (case cnt of
                                                             Nothing -> botB >> moveToSol
                                                             Just n  -> gotoLn n >> return ()))
                  ,pString "gg" >> return (LineWise, ArbMove (gotoLn 0 >> return ()))])
        ]

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
         -- eol / sol / special column
         ,(spec KHome,      sol)
         ,(char '^',        const $ ArbMove firstNonSpaceB)
         ,(char '|',        \i -> SeqMove viMoveToSol (Replicate (CharMove Forward) (i-1)))
         ,(char '$',  eol)
         ,(spec KEnd, eol)
          -- words
         ,(char 'w',       Replicate $ GenMove unitViWord (Backward,InsideBound) Forward)
         ,(char 'W',       Replicate $ GenMove unitViWORD (Backward,InsideBound) Forward)
         ,(char 'b',       Replicate $ Move unitViWord Backward)
         ,(char 'B',       Replicate $ Move unitViWORD Backward)
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
             eol   = ArbMove . viMoveToNthEol

     -- | movement *multi-chars* commands (with exclusive cut/yank semantics)
     moveCmdS_exclusive :: [(String, (Int -> ViMove))]
     moveCmdS_exclusive =
         [("[(", Replicate $ ArbMove (goUnmatchedB Backward '(' ')'))
         ,("[{", Replicate $ ArbMove (goUnmatchedB Backward '{' '}'))
         ,("])", Replicate $ ArbMove (goUnmatchedB Forward  '(' ')'))
         ,("]}", Replicate $ ArbMove (goUnmatchedB Forward  '{' '}'))]

     -- | movement commands (with inclusive cut/yank semantics)
     moveCmdFM_inclusive :: [(Event, (Int -> ViMove))]
     moveCmdFM_inclusive =
         [(char 'e',     Replicate $ GenMove unitViWord (Forward, InsideBound) Forward)
         ,(char 'E',     Replicate $ GenMove unitViWORD (Forward, InsideBound) Forward)]

     -- | movement *multi-chars* commands (with inclusive cut/yank semantics)
     moveCmdS_inclusive :: [(String, (Int -> ViMove))]
     moveCmdS_inclusive =
         [("ge", Replicate $ GenMove unitViWord (Forward, InsideBound) Backward)
         ,("gE", Replicate $ GenMove unitViWORD (Forward, InsideBound) Backward)
         ,("g_", const $ ArbMove lastNonSpaceB)]

     regionOfViMove :: ViMove -> RegionStyle -> BufferM Region
     regionOfViMove move regionStyle =
       join $ mkRegionOfStyleB <$> pointB
                               <*> (savingPointB (viMove move >> pointB))
                               <*> pure regionStyle

     -- viMove Normal Mode
     viMoveNM :: ViMove -> BufferM ()
     viMoveNM move = viMove move >> leftOnEol
     
     viMove :: ViMove -> BufferM ()
     viMove NoMove                        = return ()
     viMove (GenMove   unit boundary dir) = genMoveB unit boundary dir
     viMove (MaybeMove unit          dir) = maybeMoveB unit dir
     viMove (Move      unit          dir) = moveB unit dir
     viMove (CharMove Forward)            = moveXorEol 1
     viMove (CharMove Backward)           = moveXorSol 1
     viMove (ArbMove       move)          = move
     viMove (SeqMove move1 move2)         = viMove move1 >> viMove move2
     viMove (Replicate     move i)        = viReplicateMove move i

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
        choice $
          [c ?>>! action i | (c,action) <- singleCmdFM ] ++
          [events evs >>! action i | (evs, action) <- multiCmdFM ] ++
          [char 'r' ?>> textChar >>= write . savingPointB . writeN . replicate i
          ,pString "gt" >>! nextTabE
          ,pString "gT" >>! previousTabE]


     -- TODO: add word bounds: search for \<word\>
     searchCurrentWord :: Direction -> EditorM ()
     searchCurrentWord dir = do
       w <- withBuffer0 $ readRegionB =<< regionOfNonEmptyB unitViWord
       viSearch w [QuoteRegex] dir

     gotoTag :: Tag -> YiM ()
     gotoTag tag =
       visitTagTable $ \tagTable ->
         case lookupTag tag tagTable of
           Nothing -> fail $ "No tags containing " ++ tag
           Just (filename, line) -> do
             fnewE $ filename
             withBuffer $ gotoLn line
             return ()

     -- | Call continuation @act@ with the TagTable. Uses the global table
     -- and prompts the user if it doesn't exist
     visitTagTable :: (TagTable -> YiM ()) -> YiM ()
     visitTagTable act = do
       posTagTable <- withEditor getTags
       -- does the tagtable exist?
       case posTagTable of
         Just tagTable -> act tagTable
         Nothing -> do fps <- withEditor $ getA tagsFileListA -- withBuffer0 $ tagsFileList <$> getDynamicB
                       efps <- io $ filterM fileExist fps
                       when (null efps) $ fail ("No existing tags file among: " ++ show fps)
                       tagTable <- io $ importTagTable (head efps)
                       withEditor $ setTags tagTable
                       act tagTable

     gotoTagCurrentWord :: YiM ()
     gotoTagCurrentWord = gotoTag =<< withEditor (withBuffer0 (readRegionB =<< regionOfNonEmptyB unitViWord))

     setTagsFileList :: String -> EditorM ()
     setTagsFileList fps = resetTags >> setA tagsFileListA (split "," fps)

     -- | Parse any character that can be inserted in the text.
     textChar :: KeymapM Char
     textChar = do
       Event (KASCII c) [] <- anyEvent
       return c

     continueSearching :: (Direction -> Direction) -> EditorM ()
     continueSearching fdir = do
       m <- getRegexE
       dir <- fdir <$> getA searchDirectionA 
       printMsg $ directionElim dir '?' '/' : maybe "" fst m
       viSearch "" [] dir

     -- | cmd mode commands
     -- An event specified paired with an action that may take an integer argument.
     -- Usually the integer argument is the number of times an action should be repeated.
     singleCmdFM :: [(Event, Int -> YiM ())]
     singleCmdFM =
         [(ctrl $ char 'b',    withBuffer . upScreensB)             -- vim does (firstNonSpaceB;moveXorSol)
         ,(ctrl $ char 'f',    withBuffer . downScreensB)
         ,(ctrl $ char 'u',    withBuffer . vimScrollByB (negate . (`div` 2)))
         ,(ctrl $ char 'd',    withBuffer . vimScrollByB (`div` 2))
         ,(ctrl $ char 'g',    const viFileInfo)
         ,(ctrl $ char 'l',    const refreshEditor)
         ,(ctrl $ char 'r',    withBuffer . flip replicateM_ redoB)
         ,(ctrl $ char 'z',    const suspendEditor)
         ,(ctrl $ char ']',    const gotoTagCurrentWord)
         ,(char 'D',      withEditor . cut Exclusive . ArbMove . viMoveToNthEol)
         ,(char 'J',      const (withBuffer (moveToEol >> deleteN 1)))    -- the "\n"
         ,(char 'Y',      \n -> withEditor $ do
                                    let move = Replicate (Move Line Forward) n
                                    region <- withBuffer0 $ regionOfViMove move LineWise
                                    yankRegion LineWise region
          )
         ,(char 'U',      withBuffer . flip replicateM_ undoB)    -- NB not correct
         ,(char 'n',      const $ withEditor $ continueSearching id)
         ,(char 'N',      const $ withEditor $ continueSearching reverseDir)
         ,(char 'u',      withBuffer . flip replicateM_ undoB)

         ,(char 'X',      withEditor . cut Exclusive . (Replicate $ CharMove Backward))
         ,(char 'x',      withEditor . cut Exclusive . (Replicate $ CharMove Forward))

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
         ,(map char "ZZ", const $ viWrite >> closeWindow)
         ,(map char "ZQ", const $ closeWindow)
         ,(map char "ga", const $ viCharInfo)
         ,(map char "==", const $ withBuffer $ adjIndent IncreaseCycle)
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

     -- Argument of the 2nd component is whether the unit is outer.
     toOuter u True = leftBoundaryUnit u
     toOuter u False = u

     char2unit :: [(Char, Bool -> TextUnit)]
     char2unit =
       [('w',  toOuter unitViWord)
       ,('W',  toOuter unitViWORD)
       ,('p',  toOuter unitEmacsParagraph)
       ,('s',  toOuter unitSentence)
       ,('"',  unitDelimited '"' '"')
       ,('`',  unitDelimited '`' '`')
       ,('\'', unitDelimited '\'' '\'')
       ,('(',  unitDelimited '(' ')')
       ,(')',  unitDelimited '(' ')')
       ,('b',  unitDelimited '(' ')')
       ,('{',  unitDelimited '{' '}')
       ,('}',  unitDelimited '{' '}')
       ,('B',  unitDelimited '{' '}')
       ,('<',  unitDelimited '<' '>')
       ,('>',  unitDelimited '<' '>')
       ]

     select_any_unit :: (MonadInteract m Action Event) => (Region -> EditorM ()) -> m ()
     select_any_unit f = do 
       outer <- (char 'a' ?>> pure True) <|> (char 'i' ?>> pure False)
       choice [ char c ?>> write (f =<< withBuffer0 (regionOfNonEmptyB $ unit outer))
                | (c, unit) <- char2unit]
               

     regionOfSelection :: BufferM (RegionStyle, Region)
     regionOfSelection = do
       regionStyle <- selection2regionStyle <$> getDynamicB
       region <- join $ mkRegionOfStyleB <$> getSelectionMarkPointB
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
         adjBlock $ negate $ length txt
         deleteRegionB region
         return txt
       setRegE $ if (regionStyle == LineWise) then '\n':txt else txt
       let rowsCut = length (filter (== '\n') txt)
       when (rowsCut > 2) $ printMsg $ show rowsCut ++ " fewer lines"

     cut :: RegionStyle -> ViMove -> EditorM ()
     cut regionStyle move = do
         region <- withBuffer0 $ regionOfViMove move regionStyle
         cutRegion regionStyle region
         withBuffer0 leftOnEol

     cutSelection :: EditorM ()
     cutSelection = uncurry cutRegion =<< withBuffer0 regionOfSelection

     pasteOverSelection :: EditorM ()
     pasteOverSelection = do
       txt <- getRegE
       withBuffer0 $ do
         selStyle <- getDynamicB
         start <- getSelectionMarkPointB
         stop <- pointB
         region <- mkRegionOfStyleB start stop $ selection2regionStyle $ selStyle
         moveTo $ regionStart region
         deleteRegionB region
         insertN txt

     pasteAfter :: EditorM ()
     pasteAfter = do
       txt' <- getRegE
       withBuffer0 $ do
         adjBlock $ length txt'
         case txt' of
           '\n':txt -> moveToEol >> rightB >> insertN txt >> leftN (length txt)
           _      -> moveXorEol 1 >> insertN txt' >> leftB

     pasteBefore :: EditorM ()
     pasteBefore = do
       txt' <- getRegE
       withBuffer0 $ do
         adjBlock $ length txt'
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
     vis_single selStyle =
         choice [spec KEsc ?>> return (),
                 char 'V'  ?>> change_vis_mode selStyle (SelectionStyle Line),
                 char 'v'  ?>> change_vis_mode selStyle (SelectionStyle Character),
                 char ':'  ?>>! ex_mode ":'<,'>",
                 char 'y'  ?>>! yankSelection,
                 char 'x'  ?>>! cutSelection,
                 char 'd'  ?>>! cutSelection,
                 char 'p'  ?>>! pasteOverSelection,
                 char 's'  ?>> beginIns self (cutSelection >> withBuffer0 (setVisibleSelection False)),
                 char 'c'  ?>> beginIns self (cutSelection >> withBuffer0 (setVisibleSelection False))]


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
                 char 'i'     ?>> ins_mode self,
                 char 'I'     ?>> beginIns self firstNonSpaceB,
                 char 'a'     ?>> beginIns self $ moveXorEol 1,
                 char 'A'     ?>> beginIns self moveToEol,
                 char 'o'     ?>> beginIns self $ moveToEol >> insertB '\n',
                 char 'O'     ?>> beginIns self $ moveToSol >> insertB '\n' >> lineUp,
                 char 'c'     ?>> changeCmds,
                 char 'C'     ?>> beginIns self $ cut Exclusive viMoveToEol, -- alias of "c$" FIXME
                 char 'S'     ?>> beginIns self $ withBuffer0 moveToSol >> cut Exclusive viMoveToEol, -- non-linewise alias of "cc"
                 char 's'     ?>> beginIns self $ cut Exclusive (CharMove Forward), -- non-linewise alias of "cl"
                 char '/'     ?>>! ex_mode "/",
                 char '?'     ?>>! ex_mode "?",
                 leave,
                 spec KIns    ?>> ins_mode self]

     changeCmds :: I Event Action ()
     changeCmds =
       adjustPriority (-1) >>
         ((char 'w' ?>> change NoMove Exclusive (GenMove unitViWord (Forward, OutsideBound) Forward)) <|>
          (char 'W' ?>> change NoMove Exclusive (GenMove unitViWORD (Forward, OutsideBound) Forward))) <|>
       (char 'c' ?>> change viMoveToSol LineWise viMoveToEol) <|>
       (uncurry (change NoMove) =<< gen_cmd_move) <|>
       (select_any_unit (cutRegion Exclusive) >> ins_mode self) -- this correct while the RegionStyle is not LineWise

     change :: ViMove -> RegionStyle -> ViMove -> I Event Action ()
     change preMove regionStyle move = do
       write $ do
         withBuffer0 $ viMoveNM preMove
         cut regionStyle move
         when (regionStyle == LineWise) $ withBuffer0 (insertB '\n' >> leftB)
       ins_mode self

     ins_rep_char :: VimMode
     ins_rep_char = choice [spec KPageUp       ?>>! upScreenB,
                            spec KPageDown     ?>>! downScreenB,
                            spec KUp           ?>>! lineUp,
                            spec KDown         ?>>! lineDown,
                            spec KLeft         ?>>! moveXorSol 1,
                            spec KRight        ?>>! moveXorEol 1,
                            spec KEnd          ?>>! moveToEol,
                            spec KHome         ?>>! moveToSol,
                            spec KDel          ?>>! (adjBlock (-1) >> deleteB Character Forward),
                            spec KEnter        ?>>! insertB '\n',
                            spec KTab          ?>>! insertTabB,
                            (ctrl $ char 't')  ?>>! shiftIndentOfLine 1,
                            (ctrl $ char 'd')  ?>>! shiftIndentOfLine (-1)
                            ]

     --
     -- Some ideas for a better insert mode are contained in:
     --
     --      Poller and Garter , "A comparative study of moded and modeless
     --      text editing by experienced editor users", 1983
     --
     -- which suggest that movement commands be added to insert mode, along
     -- with delete.
     --
     def_ins_char =
            choice [spec KBS           ?>>! adjBlock (-1) >> deleteB Character Backward,
                    (ctrl $ char 'h')  ?>>! adjBlock (-1) >> deleteB Character Backward,
                    (ctrl $ char 'w')  ?>>! cut Exclusive (GenMove unitViWord (Backward,InsideBound) Backward)
                    ]
            <|> ins_rep_char
            <|| (textChar >>= write . (adjBlock 1 >>) . insertB)

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
     rep_char = choice [spec KBS           ?>>! leftB,
                        (ctrl $ char 'h')  ?>>! leftB,
                        (ctrl $ char 'w')  ?>>! genMoveB unitViWord (Backward,InsideBound) Backward
                       ] -- should undo unless pointer has been moved
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
               choice [spec KEnter       ?>>! ex_buffer_finish,
                       spec KTab         ?>>! completeMinibuffer,
                       spec KEsc         ?>>! closeBufferAndWindowE,
                       (ctrl $ char 'h') ?>>! deleteB Character Backward,
                       spec KBS          ?>>! deleteB Character Backward,
                       spec KDel         ?>>! deleteB Character Forward,
                       (ctrl $ char 'p') ?>>! historyUp,
                       spec KUp          ?>>! historyUp,
                       (ctrl $ char 'n') ?>>! historyDown,
                       spec KDown        ?>>! historyDown,
                       spec KLeft        ?>>! moveXorSol 1,
                       spec KRight       ?>>! moveXorEol 1,
                       (ctrl $ char 'w') ?>>! deleteB unitWord Backward,
                       (ctrl $ char 'u') ?>>! moveToSol >> deleteToEol]
                  <|| (textChar >>= write . insertB)
           completeMinibuffer = withBuffer elemsB >>= ex_complete >>= withBuffer . insertN
           exSimpleComplete compl s = drop (length s) <$> simpleComplete compl s
           f_complete = exSimpleComplete (matchingFileNames Nothing)
           b_complete = exSimpleComplete matchingBufferNames
           ex_complete ('e':' ':f)                             = f_complete f
           ex_complete ('e':'d':'i':'t':' ':f)                 = f_complete f
           ex_complete ('r':' ':f)                             = f_complete f
           ex_complete ('r':'e':'a':'d':' ':f)                 = f_complete f
           ex_complete ('t':'a':'b':'e':' ':f)                 = f_complete f
           ex_complete ('b':' ':f)                             = b_complete f
           ex_complete ('b':'u':'f':'f':'e':'r':' ':f)         = b_complete f
           ex_complete ('b':'d':' ':f)                         = b_complete f
           ex_complete ('b':'d':'!':' ':f)                     = b_complete f
           ex_complete ('b':'d':'e':'l':'e':'t':'e':' ':f)     = b_complete f
           ex_complete ('b':'d':'e':'l':'e':'t':'e':'!':' ':f) = b_complete f
           ex_complete ('c':'a':'b':'a':'l':' ':s)             = cabalComplete s
           ex_complete ('y':'i':' ':s)                         = exSimpleComplete (\_->getAllNamesInScope) s
           ex_complete s                                       = catchAllComplete s

           catchAllComplete = exSimpleComplete $ const $ return $ map (++ " ") $ words
                                "e edit r read tabe b buffer bd bd! bdelete bdelete! yi cabal"
           cabalComplete = exSimpleComplete $ const $ return $ cabalCmds
           cabalCmds = words "configure install list update upgrade fetch upload check sdist" ++
                       words "report build copy haddock clean hscolour register test help"

       historyStart
       spawnMinibufferE prompt (const $ ex_process)
       return ()

     -- | eval an ex command to an YiM (), also appends to the ex history
     ex_eval :: String -> YiM ()
     ex_eval cmd = do
       case cmd of
             -- regex searching
               ('/':pat) -> withEditor $ viSearch pat [] Forward
               ('?':pat) -> withEditor $ viSearch pat [] Backward

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
           {- safeQuitWindow implements the commands in vim equivalent to :q.
            - Closes the current window unless the current window is the last window on a 
            - modified buffer that is not considered "worthless".
            -}
           safeQuitWindow = do
               nw <- withBuffer needsAWindowB
               ws <- withEditor $ getA currentWindowA >>= windowsOnBufferE . bufkey
               if 1 == length ws && nw 
                 then errorEditor "No write since last change (add ! to override)"
                 else closeWindow
           
           needsAWindowB = do
             isWorthless <- gets (\b -> file b == Nothing)
             canClose <- isUnchangedB
             if isWorthless || canClose then return False else return True
           
           {- quitWindow implements the commands in vim equivalent to :q!
            - Closes the current window regardless of whether the window is on a modified
            - buffer or not. 
            - TODO: Does not quit the editor if there are modified hidden buffers.
            - 
            - Corey - Vim appears to abandon any changes to the current buffer if the window being 
            - closed is the last window on the buffer. The, now unmodified, buffer is still around 
            - and can be switched to using :b. I think this is odd and prefer the modified buffer
            - sticking around. 
            -}
           quitWindow = closeWindow
           
           {- safeQuitAllWindows implements the commands in vim equivalent to :qa!
            - Exits the editor unless there is a modified buffer that is not worthless.
            -}
           safeQuitAllWindows = do
             bs <- mapM (\b -> withEditor (withGivenBuffer0 b needsAWindowB) >>= return . (,) b) =<< readEditor bufferStack
             -- Vim only shows the first modified buffer in the error.
             case find snd bs of
               Nothing -> quitEditor
               Just (b, _) -> do
                 bufferName <- withEditor $ withGivenBuffer0 b $ gets name
                 errorEditor $ "No write since last change for buffer " 
                               ++ show bufferName
                               ++ " (add ! to override)"
           
           whenUnchanged mu f = do u <- mu
                                   if u then f
                                        else errorEditor "No write since last change (add ! to override)"


           wquitall = withAllBuffers viWrite >> quitEditor
           bdelete  = whenUnchanged (withBuffer isUnchangedB) . withEditor . closeBufferE
           bdeleteNoW = withEditor . closeBufferE

           -- fn maps from the text entered on the command line to a YiM () implementing the 
           -- command.
           fn ""           = withEditor clrStatus

           fn s | all isDigit s = do let lineNum = read s
                                     withBuffer (gotoLn lineNum) >> return ()

           fn "w"          = viWrite
           fn ('w':' ':f)  = viWriteTo $ dropSpace f
           fn "qa"         = safeQuitAllWindows
           fn "qal"        = safeQuitAllWindows
           fn "qall"       = safeQuitAllWindows
           fn "quita"      = safeQuitAllWindows
           fn "quital"     = safeQuitAllWindows
           fn "quitall"    = safeQuitAllWindows
           fn "q"          = safeQuitWindow
           fn "qu"         = safeQuitWindow
           fn "qui"        = safeQuitWindow
           fn "quit"       = safeQuitWindow
           fn "q!"         = quitWindow
           fn "qu!"        = quitWindow
           fn "qui!"       = quitWindow
           fn "quit!"      = quitWindow
           fn "qa!"        = quitEditor
           fn "quita!"     = quitEditor
           fn "quital!"    = quitEditor
           fn "quitall!"   = quitEditor
           fn "wq"         = viWrite >> closeWindow
           fn "wqa"        = wquitall
           fn "wqal"       = wquitall
           fn "wqall"      = wquitall
           fn "as"         = viCharInfo
           fn "ascii"      = viCharInfo
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
           fn "edit"       = revertE
           fn ('e':' ':f)  = fnewE f
           fn ('e':'d':'i':'t':' ':f) = fnewE f
           fn ('r':' ':f)  = withBuffer . insertN =<< io (readFile f)
           fn ('r':'e':'a':'d':' ':f) = withBuffer . insertN =<< io (readFile f)
           -- fn ('s':'e':'t':' ':'f':'t':'=':ft)  = withBuffer $ setSyntaxB $ highlighters M.! ft
           fn ('s':'e':'t':' ':'t':'a':'g':'s':'=':fps)  = withEditor $ setTagsFileList fps
           fn ('n':'e':'w':' ':f) = withEditor splitE >> fnewE f
           fn ('s':'/':cs) = withEditor $ viSub cs Line
           fn ('%':'s':'/':cs) = withEditor $ viSub cs Document

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

           fn ('t':'a':'g':' ':t) = gotoTag t

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

           fn ('c':'a':'b':'a':'l':' ':s) = cabalRun s1 (const $ return ()) (drop 1 s2) where (s1, s2) = break (==' ') s
           fn ('y':'i':' ':s) = execEditorAction s
           fn "tabnew"     = withEditor newTabE
           fn ('t':'a':'b':'e':' ':f) = do withEditor newTabE
                                           fnewE f
           fn s            = errorEditor $ "The "++show s++ " command is unknown."


     ------------------------------------------------------------------------

     --not_implemented :: Char -> YiM ()
     --not_implemented c = errorEditor $ "Not implemented: " ++ show c

     -- ---------------------------------------------------------------------
     -- Misc functions

     withAllBuffers :: YiM () -> YiM ()
     withAllBuffers m = mapM_ (\b -> withEditor (setBuffer b) >> m) =<< readEditor bufferStack

     viCharInfo :: YiM ()
     viCharInfo = do c <- withBuffer readB
                     msgEditor $ showCharInfo c ""
         where showCharInfo :: Char -> String -> String
               showCharInfo c = shows c . showChar ' ' . shows d
                              . showString ",  Hex " . showHex d
                              . showString ",  Octal " . showOct d
                 where d = ord c

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

-- | viSearch is a doSearch wrapper that print the search outcome.
viSearch :: String -> [SearchF] -> Direction -> EditorM ()
viSearch x y z = do
  r <- doSearch (if null x then Nothing else Just x) y z
  case r of
    PatternFound    -> return ()
    PatternNotFound -> printMsg "Pattern not found"
    SearchWrapped   -> printMsg "Search wrapped"


-- | Try to do a substitution
viSub :: String -> TextUnit -> EditorM ()
viSub cs unit = do
    let (pat,rep') = break (== '/')  cs
        (rep,opts) = case rep' of
                        []     -> ([],[])
                        (_:ds) -> case break (== '/') ds of
                                    (rep'', [])    -> (rep'', [])
                                    (rep'', (_:fs)) -> (rep'',fs)
    case opts of
        []    -> do_single pat rep False
        ['g'] -> do_single pat rep True
        _     -> fail ("Trailing characters " ++ show (take 10 opts)) -- TODO more options

    where do_single p r g = do
                s <- searchAndRepUnit p r g unit
                if not s then fail ("Pattern not found: "++p) else clrStatus

-- | Leave a mode. This always has priority over catch-all actions inside the mode.
leave :: VimMode
leave = oneOf [spec KEsc, ctrl $ char 'c'] >> adjustPriority (-1) >> (write clrStatus)

-- | Insert mode is either insertion actions, or the meta (\ESC) action
ins_mode :: ModeMap -> VimMode
ins_mode self = write (setStatus ("-- INSERT --", defaultStyle)) >> many (v_ins_char self <|> kwd_mode) >> leave >> write (moveXorSol 1)

beginIns :: (Show x, YiAction a x) => ModeMap -> a -> I Event Action ()
beginIns self a = write a >> ins_mode self

-- Find the item after or under the cursor and jump to its match
percentMove :: (RegionStyle, ViMove)
percentMove = (Inclusive, ArbMove tryGoingToMatch)
    where tryGoingToMatch = do
              p <- pointB
              foundMatch <- goToMatch
              when (not foundMatch) $ moveTo p
              return ()
          go dir a b = goUnmatchedB dir a b >> return True
          goToMatch = do
            c <- readB
            case c of '(' -> go Forward  '(' ')'
                      ')' -> go Backward '(' ')'
                      '{' -> go Forward  '{' '}'
                      '}' -> go Backward '{' '}'
                      '[' -> go Forward  '[' ']'
                      ']' -> go Backward '[' ']'
                      _   -> otherChar
          otherChar = do eof <- atEof
                         eol <- atEol
                         if (eof || eol)
                             then return False
                             else rightB >> goToMatch -- search for matchable character after the cursor

-- --------------------
-- | Keyword
kwd_mode :: VimMode
kwd_mode = some ((ctrl $ char 'n') ?>> (write wordComplete)) >> deprioritize >> (write resetComplete)
-- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
-- and resetting it (restarting at the 1st completion).


