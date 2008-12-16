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

import Prelude (maybe, length, filter, map, drop, break, uncurry, reads)

import Data.Char
import Data.List (nub, take, words, dropWhile, intersperse, reverse)
import Data.Maybe (fromMaybe)
import Data.Either (either)
import Data.Prototype
import Numeric (showHex, showOct)
import System.IO (readFile)
import System.Posix.Files (fileExist)

import Control.Monad.State hiding (mapM_, mapM, sequence)
import Control.Arrow hiding (left, right)

import {-# source #-} Yi.Boot
import Yi.Core
import Yi.Dired
import Yi.Eval (execEditorAction)
import Yi.File
import Yi.History
import Yi.Misc (matchingFileNames,adjBlock,adjIndent,cabalRun)
import Yi.String (dropSpace,split,lines')
import Yi.MiniBuffer
import Yi.Search
import Yi.Style
import Yi.TextCompletion
import Yi.Tag (Tag,TagTable,lookupTag,importTagTable)
import Yi.Window (bufkey)
import qualified Codec.Binary.UTF8.String as UTF8


--
-- What's missing?
--   gq,gw,fillText should leave the last \n as is.
--   fancier :s// ==> missing /c, ...
--   '.': started look at "TODO repeat" for missing things
--   @:
--   8g8
--   integrate unit transposing (transposeB) (gSaw: swap a word)
--   free keys g[bBcClLnNOSWxXyYzZ]
--   could be reused g[dDhHiQ]
--   go
--   gm, g$, g<End>, gp, gP, ]p, ]P, [P, [p, gr, gR, gs, gv, gV
--   &, :&&, g&
--   goto file [{visual}][count]g{f,F}
--   invent a variant of o and O that add spaces to be at the same col
--   :sh[ell]
--   :!!
--   movement parameterised \> \<
--   motion operators [motion.txt]: !
--   C-v: visual block mode
--   Support for marks
--   C-o and C-i: jump list
--   C-a C-@: insert prev text
--   C-k <C-K><S-Space>: insert a char using it's specification
--   C-r <reg>: insert the content of a register

-- ---------------------------------------------------------------------

type VimMode = Keymap

data ViMove = Move TextUnit Direction
            | MaybeMove TextUnit Direction
            | GenMove TextUnit (Direction, BoundarySide) Direction
            | CharMove Direction
            | PercentageFile Int
            | ArbMove (BufferM ())
            | Replicate ViMove Int
            | SeqMove ViMove ViMove
            | NoMove
  deriving (Typeable)

data ViCmd = ArbCmd (Int -> EditorM ()) Int
           | NoOp
  deriving (Typeable)

instance Initializable ViCmd where
  initial = NoOp

lastViCommandA :: Accessor Editor ViCmd
lastViCommandA = dynA

applyViCmd :: Maybe Int -> ViCmd -> EditorM ()
applyViCmd _  NoOp = return ()
applyViCmd mi (ArbCmd f i') = f $ fromMaybe i' mi

regionOfViMove :: ViMove -> RegionStyle -> BufferM Region
regionOfViMove move regionStyle =
  join $ mkRegionOfStyleB <$> pointB
                          <*> savingPointB (viMove move >> pointB)
                          <*> pure regionStyle

applyOperator :: (RegionStyle -> Region -> EditorM ()) -> Int -> (RegionStyle, ViMove) -> EditorM ()
applyOperator onRegion i (regionStyle, move) = savingCommandE f i
   where f j = onRegion regionStyle =<< withBuffer0' (regionOfViMove (Replicate move j) regionStyle)

savingCommandE :: (Int -> EditorM ()) -> Int -> EditorM ()
savingCommandE f i = putA lastViCommandA (ArbCmd f i) >> f i

savingCommandE'Y :: (Int -> EditorM ()) -> Int -> YiM ()
savingCommandE'Y f = withEditor' . savingCommandE f

savingCommandEY :: (Int -> EditorM ()) -> Int -> YiM ()
savingCommandEY f = withEditor . savingCommandE f

savingCommandB :: (Int -> BufferM ()) -> Int -> EditorM ()
savingCommandB f = savingCommandE (withBuffer0 . f)

savingCommandB' :: (Int -> BufferM ()) -> Int -> EditorM ()
savingCommandB' f = savingCommandE (withBuffer0' . f)

savingCommandB'Y :: (Int -> BufferM ()) -> Int -> YiM ()
savingCommandB'Y f = withEditor . savingCommandB' f

viMove :: ViMove -> BufferM ()
viMove NoMove                        = return ()
viMove (GenMove   unit boundary dir) = genMoveB unit boundary dir
viMove (MaybeMove unit          dir) = maybeMoveB unit dir
viMove (Move      unit          dir) = moveB unit dir
viMove (CharMove Forward)            = moveXorEol 1
viMove (CharMove Backward)           = moveXorSol 1
viMove (PercentageFile i)            = movePercentageFile i
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

movePercentageFile :: Int -> BufferM ()
movePercentageFile i = do let f :: Double
                              f  = case fromIntegral i / 100.0 of
                                      x | x > 1.0 -> 1.0
                                        | x < 0.0 -> 0.0 -- Impossible?
                                        | otherwise -> x
                          Point max_p <- sizeB
                          moveTo $ Point $ floor (fromIntegral max_p * f)
                          firstNonSpaceB

mkKeymap :: Proto ModeMap -> VimMode
mkKeymap = v_top_level . extractValue

keymap :: VimMode
keymap = mkKeymap defKeymap

-- | The Vim keymap is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate VimProcs for each phase of key input in that mode.

data ModeMap = ModeMap { -- | Top level mode
                         v_top_level :: VimMode

                         -- | vim insert mode
                       , v_ins_char :: VimMode
                       }

defKeymap :: Proto ModeMap
defKeymap = Proto template
  where 
    template self = ModeMap { v_top_level = def_top_level
                            , v_ins_char  = def_ins_char }
     where
     -- | Top level consists of simple commands that take a count arg,
     -- the replace cmd, which consumes one char of input, and commands
     -- that switch modes.
     def_top_level = do write clrStatus
                        write $ setVisibleSelection False
                        -- if the keymap "crashed" we restart here
                        -- so we clear the status line to indicate whatever mode we were in
                        -- has been left
                        choice [cmd_eval,cmd_move,cmd2other,cmd_op]

     -- | Replace mode is like insert, except it performs writes, not inserts
     -- TODO repeat
     rep_mode :: VimMode
     rep_mode = write (setStatus ("-- REPLACE --", defaultStyle)) >> many rep_char >> leaveInsRep >> write (moveXorSol 1)

     -- | Reset the selection style to a character-wise mode 'Inclusive'.
     resetSelectStyle :: BufferM ()
     resetSelectStyle = setDynamicB $ Inclusive

     -- | Visual mode, similar to command mode
     vis_move :: VimMode
     vis_move = (moveKeymap >>= write . viMove . snd)
                <|> do cnt <- count
                       choice ([events evs >>! action | (evs,action) <- visOrCmdFM ] ++
                               [events evs >>! action cnt | (evs, action) <- scrollCmdFM ])

     vis_mode :: RegionStyle -> VimMode
     vis_mode selStyle = do
       write $ do putA rectangleSelectionA $ Block == selStyle
                  setVisibleSelection True
                  pointB >>= setSelectionMarkPointB
       core_vis_mode selStyle
       write (clrStatus >> withBuffer0' (setVisibleSelection False >> resetSelectStyle))

     core_vis_mode :: RegionStyle -> VimMode
     core_vis_mode selStyle = do
       write $ do withBuffer0' $ setDynamicB $ selStyle
                  setStatus $ (msg selStyle, defaultStyle)
       many (vis_move <|>
             select_any_unit (withBuffer0' . (\r -> resetSelectStyle >> extendSelectRegionB r >> leftB)))
       visual2other selStyle
       where msg LineWise  = "-- VISUAL LINE --"
             msg Block     = "-- VISUAL BLOCK --"
             msg _         = "-- VISUAL --"

     -- | Change visual mode
     change_vis_mode :: RegionStyle -> RegionStyle -> VimMode
     change_vis_mode src dst | src == dst = return ()
                             | otherwise  = core_vis_mode dst

     -- | A KeymapM to accumulate digits.
     -- typically what is needed for integer repetition arguments to commands
     count :: KeymapM (Maybe Int)
     count = (deprioritize >> pure Nothing) <|> do
         c <- charOf id '1' '9'
         cs <- many $ charOf id '0' '9'
         return $ Just $ read (c:cs)

     viMoveToNthEol :: Int -> BufferM ()
     viMoveToNthEol n = replicateM_ n $ moveB Line Forward

     viMoveToEol :: ViMove
     viMoveToEol = MaybeMove Line Forward

     viMoveToSol :: ViMove
     viMoveToSol = MaybeMove Line Backward

     -- ---------------------------------------------------------------------
     -- | KeymapM for movement commands
     --
     -- The may be invoked directly, or sometimes as arguments to other
     -- /operator/ commands (like d).
     --

     cmd_move :: VimMode
     cmd_move = moveKeymap >>= write . withBuffer0' . viMove . snd

     -- the returned RegionStyle is used when the movement is combined with a 'cut' or 'yank'.
     moveKeymap :: KeymapM (RegionStyle, ViMove)
     moveKeymap = choice
        [ char '0' ?>> return (Exclusive, viMoveToSol)
        , char '%' ?>> return percentMove
        , do 
          cnt <- count
          let x = fromMaybe 1 cnt
          choice ([c ?>> return (Inclusive, a x) | (c,a) <- moveCmdFM_inclusive ] ++
                  [pString s >> return (Inclusive, a x) | (s,a) <- moveCmdS_inclusive ] ++
                  [c ?>> return (Exclusive, a x) | (c,a) <- moveCmdFM_exclusive ] ++
                  [events evs >> return (Exclusive, a x) | (evs,a) <- moveCmdS_exclusive ] ++
                  [c ?>> return (LineWise, a x) | (c,a) <- moveUpDownCmdFM] ++
                  [do event c; c' <- textChar; return (r, a c' x) | (c,r,a) <- move2CmdFM] ++
                  [char 'G' ?>> return (LineWise, ArbMove $ maybe (botB >> firstNonSpaceB) gotoFNS cnt)
                  ,pString "gg" >> return (LineWise, ArbMove $ gotoFNS $ fromMaybe 0 cnt)
                  ,char '\'' ?>> do c <- validMarkIdentifier
                                    return (LineWise, ArbMove $ jumpToMark c >> firstNonSpaceB)
                  ,char '`'  ?>> do c <- validMarkIdentifier
                                    return (Exclusive, ArbMove $ jumpToMark c)

                  -- The count value, in this case, is interpretted as a percentage instead of a repeat
                  -- count.
                  ,char '%' ?>> return (LineWise, PercentageFile x)])]
              where gotoFNS :: Int -> BufferM ()
                    gotoFNS n = gotoLn n >> firstNonSpaceB

     -- | movement commands (with exclusive cut/yank semantics)
     moveCmdFM_exclusive :: [(Event, (Int -> ViMove))]
     moveCmdFM_exclusive =
         -- left/right
         [(char 'h',    left)
         ,(ctrlCh 'h',  left)
         ,(spec KBS,    left)
         ,(spec KLeft,  left)
         ,(spec KRight, right)
         ,(char 'l',    right)
         ,(char ' ',    right)
         -- eol / sol / special column
         ,(spec KHome,  sol)
         ,(char '^',    const $ ArbMove firstNonSpaceB)
         ,(char '|',    ArbMove . moveToColB . pred)
         ,(char '$',    eol)
         ,(spec KEnd,   eol)
          -- words
         ,(char 'w',    Replicate $ GenMove unitViWord (Backward,InsideBound) Forward)
         ,(char 'W',    Replicate $ GenMove unitViWORD (Backward,InsideBound) Forward)
         ,(char 'b',    Replicate $ Move unitViWord Backward)
         ,(char 'B',    Replicate $ Move unitViWORD Backward)
          -- text
         ,(char '{',    Replicate $ Move unitEmacsParagraph Backward)
         ,(char '}',    Replicate $ Move unitEmacsParagraph Forward)
         ,(char '(',    Replicate $ Move unitSentence  Backward)
         ,(char ')',    Replicate $ Move unitSentence  Forward)
         ]
         where
             left  = Replicate $ CharMove Backward
             right = Replicate $ CharMove Forward
             sol   = Replicate $ viMoveToSol
             eol   = ArbMove . viMoveToNthEol

     -- | movement *multi-chars* commands (with exclusive cut/yank semantics)
     moveCmdS_exclusive :: [([Event], (Int -> ViMove))]
     moveCmdS_exclusive =
         [(map char "[(", Replicate $ ArbMove (goUnmatchedB Backward '(' ')'))
         ,(map char "[{", Replicate $ ArbMove (goUnmatchedB Backward '{' '}'))
         ,(map char "])", Replicate $ ArbMove (goUnmatchedB Forward  '(' ')'))
         ,(map char "]}", Replicate $ ArbMove (goUnmatchedB Forward  '{' '}'))
         ,(map char "gk",          up)
         ,([char 'g', spec KUp],   up)
         ,(map char "gj",          down)
         ,([char 'g', spec KDown], down)
         ]
         where
             up   = Replicate (Move VLine Backward)
             down = Replicate (Move VLine Forward)

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

     -- | up/down movement commands. these one are separated from moveCmdFM_{inclusive,exclusive}
     -- because they behave differently when yanking/cuting (line mode).
     moveUpDownCmdFM :: [(Event, Int -> ViMove)]
     moveUpDownCmdFM =
         [(char 'k',    up)
         ,(spec KUp,    up)
         ,(ctrlCh 'p',  up)
         ,(char 'j',    down)
         ,(spec KDown,  down)
         ,(ctrlCh 'j',  down)
         ,(ctrlCh 'n',  down)
         ,(spec KEnter, down)
         ,(char '-',    fns up)
         ,(char '+',    fns down)
         ,(ctrlCh 'm',  fns down)
         ,(char '_',    fns down . pred)
          -- misc
         ,(char 'H',    ArbMove . downFromTosB . pred)
         ,(char 'M',    const $ ArbMove middleB)
         ,(char 'L',    ArbMove . upFromBosB . pred)
         ]
         where
             up    = Replicate (Move VLine Backward)
             down  = Replicate (Move VLine Forward)
             fns m = (`SeqMove` ArbMove firstNonSpaceB) . m

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
        let i = fromMaybe 1 cnt
        choice $
          [events evs >>! action i   | (evs, action) <- cmdFM ] ++
          [events evs >>! action     | (evs, action) <- visOrCmdFM ] ++
          [events evs >>! action cnt | (evs, action) <- scrollCmdFM ] ++
          [char 'r' ?>> do c <- textChar
                           write $ savingCommandB (savingPointB . writeN . flip replicate c) i
          ,char 'm' ?>> setMark
          ,char '.' ?>>! applyViCmd cnt =<< getA lastViCommandA]


     -- TODO: add word bounds: search for \<word\>
     searchCurrentWord :: Direction -> EditorM ()
     searchCurrentWord dir = do
       w <- withBuffer0' $ readRegionB =<< regionOfNonEmptyB unitViWord
       viSearch w [QuoteRegex] dir

     gotoTag :: Tag -> YiM ()
     gotoTag tag =
       visitTagTable $ \tagTable ->
         case lookupTag tag tagTable of
           Nothing -> fail $ "No tags containing " ++ tag
           Just (filename, line) -> do
             viFnewE filename
             withBuffer' $ gotoLn line
             return ()

     -- | Call continuation @act@ with the TagTable. Uses the global table
     -- and prompts the user if it doesn't exist
     visitTagTable :: (TagTable -> YiM ()) -> YiM ()
     visitTagTable act = do
       posTagTable <- withEditor getTags
       -- does the tagtable exist?
       case posTagTable of
         Just tagTable -> act tagTable
         Nothing -> do fps <- withEditor $ getA tagsFileListA -- withBuffer0' $ tagsFileList <$> getDynamicB
                       efps <- io $ filterM fileExist fps
                       when (null efps) $ fail ("No existing tags file among: " ++ show fps)
                       tagTable <- io $ importTagTable (head efps)
                       withEditor $ setTags tagTable
                       act tagTable

     gotoTagCurrentWord :: YiM ()
     gotoTagCurrentWord = gotoTag =<< withEditor (withBuffer0' (readRegionB =<< regionOfNonEmptyB unitViWord))

     setTagsFileList :: String -> EditorM ()
     setTagsFileList fps = resetTags >> putA tagsFileListA (split "," fps)

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

     skippingFirst :: ([a] -> [a]) -> [a] -> [a]
     skippingFirst f = list [] (\x -> (x :) . f)

     skippingLast :: ([a] -> [a]) -> [a] -> [a]
     skippingLast f xs = f (init xs) ++ [last xs]

     skippingNull :: ([a] -> [b]) -> [a] -> [b]
     skippingNull _ [] = []
     skippingNull f xs = f xs

     joinLinesB :: Region -> BufferM ()
     joinLinesB =
       savingPointB .
         (modifyRegionB $ skippingLast $
            concat . (skippingFirst $ map $ skippingNull ((' ':) . dropWhile isSpace)) . lines')

     concatLinesB :: Region -> BufferM ()
     concatLinesB = savingPointB . (modifyRegionB $ skippingLast $ filter (/='\n'))

     onCurrentWord :: (String -> String) -> BufferM ()
     onCurrentWord f = modifyRegionB f =<< regionOfNonEmptyB unitViWord

     onNumberInString :: (Read a, Show a, Num a) => (a -> a) -> String -> String
     onNumberInString f s = case reads s2 of
         []          -> s
         (n, rest):_ -> s1 ++ show (f n) ++ rest
       where (s1,s2) = break isDigit s

     -- as cmdFM but these commands are also valid in visual mode
     visOrCmdFM :: [([Event], YiM ())]
     visOrCmdFM =
         [([ctrlCh 'l'],      userForceRefresh)
         ,([ctrlCh 'z'],      suspendEditor)
         ,([ctrlCh ']'],      gotoTagCurrentWord)
         ] ++
         map (second withEditor)
         [([ctrlW, char 'c'], tryCloseE)
         ,([ctrlW, char 'o'], closeOtherE)
         ,([ctrlW, char 's'], splitE)
         ,([ctrlW, char 'w'], nextWinE)
         ,([ctrlW, char 'W'], prevWinE)
         ,([ctrlW, char 'p'], prevWinE)

         -- these 4 commands should go to moveKeymap
         -- however moveKeymap is currently confined to BufferM
         ,([char 'n'],        continueSearching id)
         ,([char 'N'],        continueSearching reverseDir)
         ,([char '*'],        searchCurrentWord Forward)
         ,([char '#'],        searchCurrentWord Backward)

         -- since we don't have vertical splitting,
         -- these moving can be done using next/prev.
         ,([ctrlW,spec KDown],  nextWinE)
         ,([ctrlW,spec KUp],    prevWinE)
         ,([ctrlW,spec KRight], nextWinE)
         ,([ctrlW,spec KLeft],  prevWinE)
         ,([ctrlW,char 'k'],    prevWinE)
         ,([ctrlW,char 'j'],    nextWinE)    -- Same as the above pair, when you're a bit slow to release ctl.
         ,([ctrlW, ctrlCh 'k'], prevWinE)
         ,([ctrlW, ctrlCh 'j'], nextWinE)
         ,(map char "ga",       viCharInfo)
         ,(map char "g8",       viChar8Info)
         ,(map char "gt",       nextTabE)
         ,(map char "gT",       previousTabE)
         ]

     -- | cmd mode commands
     -- An event specified paired with an action that may take an integer argument.
     -- Usually the integer argument is the number of times an action should be repeated.
     cmdFM :: [([Event], Int -> YiM ())]
     cmdFM =
         [([ctrlCh 'g'],    const $ withEditor viFileInfo)

         -- undo/redo
         ,([char 'u'],      withBuffer' . flip replicateM_ undoB)
         ,([char 'U'],      withBuffer' . flip replicateM_ undoB)    -- NB not correct
         ,([ctrlCh 'r'],    withBuffer' . flip replicateM_ redoB)

         ,([ctrlCh 'a'],    savingCommandB'Y $ onCurrentWord . onNumberInString . (+))
         ,([ctrlCh 'x'],    savingCommandB'Y $ onCurrentWord . onNumberInString . flip (-))
         ,([char 'D'],      savingCommandE'Y $ cut Exclusive . ArbMove . viMoveToNthEol)
         ,([char 'J'],      savingCommandB'Y $ (joinLinesB =<<) . countLinesRegion . max 2)
         ,(map char "gJ",   savingCommandB'Y $ (concatLinesB =<<) . countLinesRegion . max 2)
         ,([char 'Y'],      withEditor . yank LineWise . (Replicate $ Move Line Forward))
         ,([char 'X'],      savingCommandE'Y $ cut Exclusive . (Replicate $ CharMove Backward))
         ,([char 'x'],      savingCommandE'Y $ cut Exclusive . (Replicate $ CharMove Forward))

         -- pasting
         ,([char 'p'],      savingCommandEY $ flip replicateM_ pasteAfter)
         ,([char 'P'],      savingCommandEY $ flip replicateM_ pasteBefore)

         ,([char '~'],      savingCommandB'Y $ \i -> do -- TODO cleanup
                              p <- pointB
                              moveXorEol i
                              q <- pointB
                              moveTo p
                              mapRegionB (mkRegion p q) switchCaseChar
                              moveTo q)
         ,(map char "ZZ",   const $ viWriteModified >> closeWindow)
         ,(map char "ZQ",   const $ closeWindow)
         ]

     ctrlW :: Event
     ctrlW = ctrlCh 'w'

     scrollCmdFM :: [([Event], Maybe Int -> BufferM ())]
     scrollCmdFM =
         [([ctrlCh 'b'],            upScreensB . fromMaybe 1)   -- vim does (firstNonSpaceB;moveXorSol)
         ,([ctrlCh 'f'],            downScreensB . fromMaybe 1)
         ,([ctrlCh 'u'],            vimScrollByB (negate . (`div` 2)) . fromMaybe 1)
         ,([ctrlCh 'd'],            vimScrollByB (`div` 2) . fromMaybe 1)
         ,([ctrlCh 'y'],            vimScrollB . negate . fromMaybe 1)
         ,([ctrlCh 'e'],            vimScrollB . fromMaybe 1)
         ,([spec KPageUp],          upScreensB . fromMaybe 1)
         ,([spec KPageDown],        downScreensB . fromMaybe 1)
         ,([char 'z', spec KEnter], mmGoFNS scrollCursorToTopB)
         ,(map char "zt",           mmGoSC  scrollCursorToTopB)
         ,(map char "z.",           mmGoFNS scrollToCursorB)
         ,(map char "zz",           mmGoSC  scrollToCursorB)
         ,(map char "z-",           mmGoFNS scrollCursorToBottomB)
         ,(map char "zb",           mmGoSC  scrollCursorToBottomB)]
             where mayMove :: BufferM () -> Maybe Int -> BufferM ()
                   mayMove scroll cnt = do
                      case cnt of
                         Just n -> gotoLn n >> return ()
                         Nothing -> return ()
                      scroll
                   mmGoFNS scroll = mayMove (scroll >> firstNonSpaceB)
                   mmGoSC  scroll = movingToPrefCol . mayMove scroll

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
       let i = fromMaybe 1 cnt
       choice $ [let s1 = prefix [c]
                     ss = nub [[c], s1]
                     onRegion = onRegion' 1
                 in
                 pString s1 >>
                    choice ([ forceRegStyle >>= \ frs -> moveKeymap >>= write . applyOperator onRegion i . first frs -- TODO: text units (eg. dViB)
                            , select_any_unit (onRegion Exclusive) ] ++ -- TODO repeat
                            [ pString s >>! applyOperator onRegion (i-1) (LineWise, Move VLine Forward) | s <- ss ]
                           )

                | (prefix,_,c,onRegion') <- operators, c /= 'J'
                ]
         where
             -- | Forces RegionStyle; see motion.txt, line 116 and below (Vim 7.2)
             forceRegStyle = do
                 style <- many $ choice [ char 'V' ?>> return (const LineWise)
                                        , char 'v' ?>> return swpRsOrIncl
                                        , ctrlCh 'v' ?>> return (const Block) ]
                 return $ last (id:style)
                            where swpRsOrIncl Exclusive = Inclusive
                                  swpRsOrIncl _         = Exclusive

     -- | operator (i.e. movement-parameterised) actions
     operators :: [((String->String), (String->String), Char, (Int -> RegionStyle -> Region -> EditorM ()))]
     operators = [ (id, id, 'd', const $ \s r -> cutRegion s r >> withBuffer0 leftOnEol)
                 , (id, id, 'y', const $ nonBlockRegion "y" yankRegion)
                 , (id, id, '=', const $ mapRegions_ indentRegion)
                 , (id, id, '>', mapRegions_ . shiftIndentOfRegion)
                 , (id, id, '<', mapRegions_ . shiftIndentOfRegion . negate)
                 , (id, id, 'J', const $ nonBlockRegion "J" (const $ withBuffer0' . joinLinesB))
                 , (g_, g_, 'J', const $ nonBlockRegion "gJ" (const $ withBuffer0' . concatLinesB))
                 , (g_, id, '~', const $ viMapRegion switchCaseChar)
                 , (g_, id, 'u', const $ viMapRegion toLower)
                 , (g_, id, 'U', const $ viMapRegion toUpper)
                 , (g_, g_, '?', const $ viMapRegion rot13Char)
                 , (g_, g_, 'q', const $ nonBlockRegion "gq" (const $ withBuffer0' . fillRegion))
                 , (g_, g_, 'w', const $ nonBlockRegion "gw" (const $ withBuffer0' . savingPointB . fillRegion))
                 ]
        where g_ = ('g':)
              nonBlockRegion n _  Block _ = fail (show n ++ " does not works yet for block selections")
              nonBlockRegion _ op s     r = op s r
              mapRegions_ f Block r = withBuffer0' $ mapM_ f =<< blockifyRegion r
              mapRegions_ f _     r = withBuffer0' $ f r

     toOuter outer _     True  = leftBoundaryUnit outer
     toOuter _     inner False = inner

     char2unit :: [(Char, Bool -> TextUnit)]
     char2unit =
       [('w',  toOuter unitViWord unitViWordAnyBnd)
       ,('W',  toOuter unitViWORD unitViWORDAnyBnd)
       ,('p',  toOuter unitEmacsParagraph unitEmacsParagraph) -- TODO inner could be inproved
       ,('s',  toOuter unitSentence unitSentence) -- TODO inner could be inproved
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
       choice [ char c ?>> write (f =<< withBuffer0' (regionOfNonEmptyB $ unit outer))
              | (c, unit) <- char2unit]


     regionOfSelection :: BufferM (RegionStyle, Region)
     regionOfSelection = do
       setMarkHere '>'
       regionStyle <- getA regionStyleA
       region <- join $ mkRegionOfStyleB <$> getSelectionMarkPointB
                                         <*> pointB
                                         <*> pure regionStyle
       return (regionStyle, region)

     indentRegion :: Region -> BufferM ()
     indentRegion region = do
       len <- length . filter (=='\n') <$> readRegionB region
       savingPointB $ do
         moveTo $ regionStart region
         replicateM_ len $ adjIndent IncreaseCycle >> lineDown
       firstNonSpaceB

     yankRegion :: RegionStyle -> Region -> EditorM ()
     yankRegion regionStyle region | regionIsEmpty region = return ()
                                   | otherwise            = do
       when (regionStyle == Block) $ fail "yankRegion does not work on block regions"
       txt <- withBuffer0' $ readRegionB region
       setRegE $ if (regionStyle == LineWise) then '\n':txt else txt
       let rowsYanked = length (filter (== '\n') txt)
       when (rowsYanked > 2) $ printMsg $ show rowsYanked ++ " lines yanked"

     yank :: RegionStyle -> ViMove -> EditorM ()
     yank regionStyle move =
       yankRegion regionStyle =<< (withBuffer0' $ regionOfViMove move regionStyle)

     cutRegion :: RegionStyle -> Region -> EditorM ()
     cutRegion Block region = do withBuffer0' $ mapM_ deleteRegionB =<< reverse <$> blockifyRegion region
                                 printMsg "This block region is not cut just deleted"
     cutRegion regionStyle region | regionIsEmpty region = return ()
                                  | otherwise            = do
       (txt, rowsCut) <- withBuffer0 $ do
         txt <- readRegionB region
         let rowsCut = length $ filter (=='\n') txt
         when (rowsCut==0) $ replicateM_ (length txt) (adjBlock (-1))
         deleteRegionB region
         return (txt, rowsCut)
       setRegE $ if (regionStyle == LineWise) then '\n':txt else txt

       when (rowsCut > 2) $ printMsg $ show rowsCut ++ " fewer lines"

     cut :: RegionStyle -> ViMove -> EditorM ()
     cut regionStyle move = do
         region <- withBuffer0 $ regionOfViMove move regionStyle
         cutRegion regionStyle region

     cutSelection :: EditorM ()
     cutSelection = uncurry cutRegion =<< withBuffer0' regionOfSelection

     pasteOverSelection :: EditorM ()
     pasteOverSelection = do
       txt <- getRegE
       withBuffer0' $ do
         regStyle <- getA regionStyleA
         start    <- getSelectionMarkPointB
         stop     <- pointB
         region   <- mkRegionOfStyleB start stop regStyle
         moveTo $ regionStart region
         deleteRegionB region
         insertN txt

     pasteAfter :: EditorM ()
     pasteAfter = do
       txt' <- getRegE
       withBuffer0' $ do
         when ('\n' `notElem` txt') $ adjBlock $ length txt'
         case txt' of
           '\n':txt -> moveToEol >> rightB >> insertN txt >> leftN (length txt)
           _        -> moveXorEol 1 >> insertN txt' >> leftB

     pasteBefore :: EditorM ()
     pasteBefore = do
       txt' <- getRegE
       withBuffer0' $ do
         when ('\n' `notElem` txt') $ adjBlock $ length txt'
         case txt' of
           '\n':txt -> moveToSol >> insertN txt >> leftN (length txt)
           _        -> insertN txt' >> leftB

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
     viMapRegion f Block region = withBuffer0' $ mapM_ (`mapRegionB` f) =<< blockifyRegion region
     viMapRegion f _     region = withBuffer0' $ mapRegionB region f

     countLinesRegion :: Int -> BufferM Region
     countLinesRegion n = regionOfViMove (Replicate (Move VLine Forward) (n - 1)) LineWise

     -- | Switching to another mode from visual mode.
     --
     -- All visual commands are meta actions, as they transfer control to another
     -- KeymapM. In this way vis_single is analogous to cmd2other
     --
     visual2other :: RegionStyle -> VimMode
     visual2other selStyle = do
         cnt <- count
         let i = fromMaybe 1 cnt
         choice $ [spec KEsc ?>> return ()
                  ,char 'V'  ?>> change_vis_mode selStyle LineWise
                  ,char 'v'  ?>> change_vis_mode selStyle Inclusive
                  ,ctrlCh 'v'?>> change_vis_mode selStyle Block
                  ,char ':'  ?>>! ex_mode ":'<,'>"
                  ,char 'p'  ?>>! pasteOverSelection -- TODO repeat
                  ,char 'x'  ?>>! (cutSelection >> withBuffer0 leftOnEol) -- TODO repeat
                  ,char 's'  ?>> beginIns self (cutSelection >> withBuffer0 (setVisibleSelection False)) -- TODO repeat
                  ,char 'c'  ?>> beginIns self (cutSelection >> withBuffer0 (setVisibleSelection False)) -- TODO repeat
                  ,char 'r'  ?>> do x <- textChar -- TODO repeat
                                    let convert '\n' = '\n'
                                        convert  _   = x
                                    write $ uncurry (viMapRegion convert) =<< withBuffer0 regionOfSelection
                  ] ++
                  [pString (prefix [c]) >>! (uncurry (action i) =<< withBuffer0' regionOfSelection) -- TODO repeat
                  | (_, prefix, c, action) <- operators ]

     -- | Switch to another vim mode from command mode.
     --
     -- These commands are meta actions, as they transfer control to another
     -- KeymapM. Some of these commands also perform an action before switching.
     --
     cmd2other :: VimMode
     cmd2other =
         choice [char ':'     ?>>! ex_mode ":",
                 char 'v'     ?>> vis_mode Inclusive,
                 char 'V'     ?>> vis_mode LineWise,
                 ctrlCh 'v'   ?>> vis_mode Block, -- one use VLine for block mode
                 char 'R'     ?>> rep_mode,
                 char 'i'     ?>> ins_mode self,
                 char 'I'     ?>> beginIns self firstNonSpaceB,
                 pString "gi"  >> beginIns self (jumpToMark '^'),
                 pString "gI"  >> beginIns self moveToSol,
                 char 'a'     ?>> beginIns self $ moveXorEol 1,
                 char 'A'     ?>> beginIns self moveToEol,
                 char 'o'     ?>> beginIns self $ moveToEol >> insertB '\n',
                 char 'O'     ?>> beginIns self $ moveToSol >> insertB '\n' >> lineUp,
                 char 'c'     ?>> changeCmds,

                 -- FIXME: those two should take int argument
                 char 'C'     ?>> beginIns self $ cut Exclusive viMoveToEol, -- alias of "c$" -- TODO repeat
                 char 'S'     ?>> beginIns self $ withBuffer0' moveToSol >> cut Exclusive viMoveToEol, -- non-linewise alias of "cc" -- TODO repeat
                 char 's'     ?>> beginIns self $ cut Exclusive (CharMove Forward), -- non-linewise alias of "cl" -- TODO repeat
                 char '/'     ?>>! ex_mode "/",
                 char '?'     ?>>! ex_mode "?",
                 leave,
                 spec KIns    ?>> ins_mode self]

     -- TODO repeat
     changeCmds :: I Event Action ()
     changeCmds =
       adjustPriority (-1) >>
         ((char 'w' ?>> change NoMove Exclusive (GenMove unitViWord (Forward, OutsideBound) Forward)) <|>
          (char 'W' ?>> change NoMove Exclusive (GenMove unitViWORD (Forward, OutsideBound) Forward))) <|>
       (char 'c' ?>> change viMoveToSol LineWise viMoveToEol) <|>
       (uncurry (change NoMove) =<< moveKeymap) <|>
       (select_any_unit (cutRegion Exclusive) >> ins_mode self) -- this correct while the RegionStyle is not LineWise

     change :: ViMove -> RegionStyle -> ViMove -> I Event Action ()
     change preMove regionStyle move = do
       write $ do
         withBuffer0' $ viMove preMove
         cut regionStyle move
         when (regionStyle == LineWise) $ withBuffer0' $ insertB '\n' >> leftB
       ins_mode self

     -- The Vim semantics is a little different here, When receiving CTRL-D
     -- instead of looking at the last typed character, one look at the previous
     -- character in buffer and if it's '0' then one delete the indentation.
     -- This means that one are sensible to lines already containing a '0'.
     -- I consider this to be very minor issue.
     dedentOrDeleteIndent :: BufferM ()
     dedentOrDeleteIndent = do
       c <- savingPointB (moveXorSol 1 >> readB)
       r <- regionOfB Line
       if c == '0' then deleteB Character Backward >> deleteIndentOfRegion r
                   else shiftIndentOfRegion (-1) r

     upTo :: Alternative f => f a -> Int -> f [a]
     _ `upTo` 0 = empty
     p `upTo` n = (:) <$> p <*> (p `upTo` pred n <|> pure []) 

     insertSpecialChar :: (Char -> BufferM ()) -> VimMode
     insertSpecialChar insrepB =
          insertNumber insrepB
      <|> (ctrlCh '@' ?>>! insrepB '\000')
      <|| (write . withBuffer0' . insrepB . eventToChar =<< anyEvent)

     insertNumber :: (Char -> BufferM ()) -> VimMode
     insertNumber insrepB = do
         choice [g [charOf id '0' '1',dec,dec] ""
                ,g [charOf id '2' '2',charOf id '0' '5',dec] ""
                ,g [charOf id '2' '2',charOf id '6' '9'] ""
                ,g [charOf id '3' '9',dec] ""
                ,oneOf (map char "oO") >> g [charOf id '0' '3',oct,oct] "0o"
                ,oneOf (map char "oO") >> g [charOf id '4' '7',oct] "0o"
                ,oneOf (map char "xX") >> g [hex,hex] "0x"
                -- NP: I don't get why this does not work (ex typing "i<CTRL-Q>u3b1.")
                -- ,char 'u' ?>> f (hex `upTo` 4) "0x"
                ,char 'u' ?>> f (sequence $ replicate 4 hex) "0x"
                ,char 'U' ?>> f (sequence $ replicate 8 hex) "0x"]
       where dec = charOf id '0' '9'
             oct = charOf id '0' '7'
             hex = charOf id '0' '9' <|> charOf id 'a' 'f' <|> charOf id 'A' 'F'
             f digits prefix = do xs <- digits
                                  write $ withBuffer0' $ insrepB $ chr $ read $ prefix ++ xs
             g digits prefix = f (sequence digits) prefix

     ins_rep_char :: (Char -> BufferM ()) -> VimMode
     ins_rep_char insrepB =
       choice [spec KPageUp   ?>>! upScreenB
              ,spec KPageDown ?>>! downScreenB
              ,spec KUp       ?>>! lineUp
              ,spec KDown     ?>>! lineDown
              ,spec KLeft     ?>>! moveXorSol 1
              ,spec KRight    ?>>! moveXorEol 1
              ,spec KEnd      ?>>! moveToEol
              ,spec KHome     ?>>! moveToSol
              ,spec KDel      ?>>! (adjBlock (-1) >> deleteB Character Forward)
              ,spec KEnter    ?>>! insertB '\n'
              ,ctrlCh 'j'     ?>>! insertB '\n'
              ,ctrlCh 'm'     ?>>! insertB '\r'
              ,spec KTab      ?>>! mapM_ insrepB =<< tabB
              ,ctrlCh 'i'     ?>>! mapM_ insrepB =<< tabB
              ,ctrlCh 'e'     ?>>! insrepB =<< savingPointB (lineDown >> readB) -- TODO repeat: not symbolically reminded, just the inserted char
              ,ctrlCh 'y'     ?>>! insrepB =<< savingPointB (lineUp >> readB) -- IDEM
              ,ctrlCh 't'     ?>>! shiftIndentOfRegion 1 =<< regionOfB Line
              ,ctrlCh 'd'     ?>>! withBuffer0' dedentOrDeleteIndent
              ,ctrlCh 'v'     ?>>  insertSpecialChar insrepB
              ,ctrlCh 'q'     ?>>  insertSpecialChar insrepB
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
     -- Which is fine in Vim (and so in Yi too) since there is a bunch of
     -- handy bindings to edit while composing (backspace, C-W, C-T, C-D, C-E, C-Y...)
     --
     def_ins_char =
            choice [spec KBS   ?>>! adjBlock (-1) >> deleteB Character Backward
                   ,ctrlCh 'h' ?>>! adjBlock (-1) >> deleteB Character Backward
                   ,ctrlCh 'w' ?>>! deleteRegionB =<< regionOfPartNonEmptyB unitViWordOnLine Backward
                   ]
            <|> ins_rep_char insertB
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
     rep_char = choice [spec KBS   ?>>! leftB
                       ,ctrlCh 'h' ?>>! leftB
                       ,ctrlCh 'w' ?>>! genMoveB unitViWord (Backward,InsideBound) Backward
                       ] -- should undo unless pointer has been moved
                <|> ins_rep_char replaceB
                <|| do c <- textChar; write $ replaceB c
        where replaceB c = do e <- atEol; if e then insertB c else writeB c

     -- ---------------------------------------------------------------------
     -- Ex mode. We also process regex searching mode here.

     ex_mode :: String -> EditorM ()
     ex_mode prompt = do
       -- The above ensures that the action is performed on the buffer that originated the minibuffer.
       let ex_buffer_finish = do
             withEditor $ historyFinish
             lineString <- withBuffer' elemsB
             withEditor closeBufferAndWindowE
             ex_eval (head prompt : lineString)
           ex_process :: VimMode
           ex_process =
               choice [spec KEnter ?>>! ex_buffer_finish
                      ,spec KTab   ?>>! completeMinibuffer
                      ,spec KEsc   ?>>! closeBufferAndWindowE
                      ,ctrlCh 'h'  ?>>! deleteB Character Backward
                      ,spec KBS    ?>>! deleteB Character Backward
                      ,spec KDel   ?>>! deleteB Character Forward
                      ,ctrlCh 'p'  ?>>! historyUp
                      ,spec KUp    ?>>! historyUp
                      ,ctrlCh 'n'  ?>>! historyDown
                      ,spec KDown  ?>>! historyDown
                      ,spec KLeft  ?>>! moveXorSol 1
                      ,spec KRight ?>>! moveXorEol 1
                      ,ctrlCh 'w'  ?>>! deleteB unitWord Backward
                      ,ctrlCh 'u'  ?>>! moveToSol >> deleteToEol]
                  <|| (textChar >>= write . insertB)
           completeMinibuffer = withBuffer elemsB >>= ex_complete >>= withBuffer . insertN
           exSimpleComplete compl s' = let s = dropWhile isSpace s' in drop (length s) <$> simpleComplete compl s
           f_complete = exSimpleComplete (matchingFileNames Nothing)
           b_complete = exSimpleComplete matchingBufferNames
           ex_complete ('e':' ':f)                             = f_complete f
           ex_complete ('e':'d':'i':'t':' ':f)                 = f_complete f
           ex_complete ('w':' ':f)                             = f_complete f
           ex_complete ('w':'r':'i':'t':'e':' ':f)             = f_complete f
           ex_complete ('r':' ':f)                             = f_complete f
           ex_complete ('r':'e':'a':'d':' ':f)                 = f_complete f
           ex_complete ('t':'a':'b':'e':' ':f)                 = f_complete f
           ex_complete ('s':'a':'v':'e':'a':'s':' ':f)         = f_complete f
           ex_complete ('s':'a':'v':'e':'a':'s':'!':' ':f)     = f_complete f
           ex_complete ('b':' ':f)                             = b_complete f
           ex_complete ('b':'u':'f':'f':'e':'r':' ':f)         = b_complete f
           ex_complete ('b':'d':' ':f)                         = b_complete f
           ex_complete ('b':'d':'!':' ':f)                     = b_complete f
           ex_complete ('b':'d':'e':'l':'e':'t':'e':' ':f)     = b_complete f
           ex_complete ('b':'d':'e':'l':'e':'t':'e':'!':' ':f) = b_complete f
           ex_complete ('c':'a':'b':'a':'l':' ':s)             = cabalComplete s
           ex_complete ('s':'e':'t':' ':'f':'t':'=':f)         = completeModes f
           ex_complete ('y':'i':' ':s)                         = exSimpleComplete (\_->getAllNamesInScope) s
           ex_complete s                                       = catchAllComplete s

           catchAllComplete = exSimpleComplete $ const $ return $
                                ("set ft=" :) $ map (++ " ") $ words
                                "e edit r read saveas saveas! tabe b buffer bd bd! bdelete bdelete! yi cabal nohlsearch"
           cabalComplete = exSimpleComplete $ const $ return $ cabalCmds
           cabalCmds = words "configure install list update upgrade fetch upload check sdist" ++
                       words "report build copy haddock clean hscolour register test help"
           completeModes = exSimpleComplete (const $ getAllModeNames)

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
               nw <- withBuffer' needsAWindowB
               ws <- withEditor $ getA currentWindowA >>= windowsOnBufferE . bufkey
               if 1 == length ws && nw 
                 then errorEditor "No write since last change (add ! to override)"
                 else closeWindow
           
           needsAWindowB = do
             isWorthless <- gets (either (const True) (const False) . (^. identA))
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
                 bufferName <- withEditor $ withGivenBuffer0 b $ gets file
                 errorEditor $ "No write since last change for buffer " 
                               ++ show bufferName
                               ++ " (add ! to override)"
           
           whenUnchanged mu f = do u <- mu
                                   if u then f
                                        else errorEditor "No write since last change (add ! to override)"


           wquitall = forAllBuffers fwriteBufferE >> quitEditor
           bdelete  = whenUnchanged (withBuffer' isUnchangedB) . withEditor . closeBufferE . dropSpace
           bdeleteNoW = withEditor . closeBufferE . dropSpace

           -- fn maps from the text entered on the command line to a YiM () implementing the 
           -- command.
           fn ""           = withEditor clrStatus

           fn s | all isDigit s = withBuffer' (gotoLn (read s) >> firstNonSpaceB)

           fn "w"          = viWrite
           fn ('w':' ':f)  = viSafeWriteTo $ dropSpace f
           fn ('w':'!':' ':f)  = viWriteTo $ dropSpace f
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
           fn "as"         = withEditor viCharInfo
           fn "ascii"      = withEditor viCharInfo
           fn "x"          = viWriteModified >> closeWindow
           fn "xi"         = viWriteModified >> closeWindow
           fn "xit"        = viWriteModified >> closeWindow
           fn "exi"        = viWriteModified >> closeWindow
           fn "exit"       = viWriteModified >> closeWindow
           fn "n"          = withEditor nextBufW
           fn "next"       = withEditor nextBufW
           fn "$"          = withBuffer' botB
           fn "p"          = withEditor prevBufW
           fn "prev"       = withEditor prevBufW
           fn ('s':'p':_)  = withEditor splitE
           fn "e"          = revertE
           fn "edit"       = revertE
           fn ('e':' ':f)  = viFnewE f
           fn ('e':'d':'i':'t':' ':f) = viFnewE f
           fn ('s':'a':'v':'e':'a':'s':' ':f)     = let f' = dropSpace f in viSafeWriteTo f' >> fnewE f'
           fn ('s':'a':'v':'e':'a':'s':'!':' ':f) = let f' = dropSpace f in viWriteTo f' >> fnewE f'
           fn ('r':' ':f)  = withBuffer' . insertN =<< io (readFile $ dropSpace f)
           fn ('r':'e':'a':'d':' ':f) = withBuffer' . insertN =<< io (readFile $ dropSpace f)
           fn ('s':'e':'t':' ':'f':'t':'=':ft)  = do (AnyMode m) <- anyModeByName (dropSpace ft) ; withBuffer $ setMode m
           fn ('s':'e':'t':' ':'t':'a':'g':'s':'=':fps)  = withEditor $ setTagsFileList fps
           fn ('n':'e':'w':' ':f) = withEditor splitE >> viFnewE f
           fn ('s':'/':cs) = withEditor $ viSub cs Line
           fn ('%':'s':'/':cs) = withEditor $ viSub cs Document

           fn ('b':' ':"m") = withEditor $ switchToBufferWithNameE "*messages*"
           fn ('b':' ':f)   = withEditor $ switchToBufferWithNameE $ dropSpace f
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
                 ln  <- withBuffer' readLnB
                 ln' <- runProcessWithInput f ln
                 withBuffer' $ do moveToSol
                                  deleteToEol
                                  insertN ln'
                                  moveToSol

     --    Needs to occur in another buffer
     --    fn ('!':f) = runProcessWithInput f []

           fn "reload"     = reloadEditor >> return ()    -- not in vim

           fn "redr"       = userForceRefresh
           fn "redraw"     = userForceRefresh

           fn "u"          = withBuffer' undoB
           fn "undo"       = withBuffer' undoB
           fn "red"        = withBuffer' redoB
           fn "redo"       = withBuffer' redoB

           fn "sus"        = suspendEditor
           fn "suspend"    = suspendEditor
           fn "st"         = suspendEditor
           fn "stop"       = suspendEditor

           fn ('c':'a':'b':'a':'l':' ':s) = cabalRun s1 (const $ return ()) (drop 1 s2) where (s1, s2) = break (==' ') s
           fn ('y':'i':' ':s) = execEditorAction $ dropSpace s
           fn "tabm"       = withEditor (moveTab Nothing)
           fn ('t':'a':'b':'m':' ':n) = withEditor (moveTab $ Just (read n))
           fn "tabnew"     = withEditor newTabE
           fn ('t':'a':'b':'e':' ':f) = withEditor newTabE >> viFnewE f
           fn "noh"        = withEditor resetRegexE
           fn "nohlsearch" = withEditor resetRegexE
           fn s            = errorEditor $ "The "++show s++ " command is unknown."


     ------------------------------------------------------------------------

     --not_implemented :: Char -> YiM ()
     --not_implemented c = errorEditor $ "Not implemented: " ++ show c

     -- ---------------------------------------------------------------------
     -- Misc functions

     forAllBuffers :: (BufferRef -> YiM ()) -> YiM ()
     forAllBuffers f = mapM_ f =<< readEditor bufferStack

     viCharInfo :: EditorM ()
     viCharInfo = do c <- withBuffer0' readB
                     printMsg $ showCharInfo c ""
         where showCharInfo :: Char -> ShowS
               showCharInfo c = shows c . showChar ' ' . shows d
                              . showString ",  Hex " . showHex d
                              . showString ",  Octal " . showOct d
                 where d = ord c

     viChar8Info :: EditorM ()
     viChar8Info = do c <- withBuffer0' readB
                      let w8 = UTF8.encode [c]
                      printMsg $ shows c . showChar ' ' . showSeq shows w8
                               . showString ",  Hex " . showSeq showHex w8
                               . showString ",  Octal " . showSeq showOct w8 $ ""
         where showSeq showX xs s = foldr ($) s $ intersperse (showChar ' ') $ map showX $ xs

     viFileInfo :: EditorM ()
     viFileInfo =
         do bufInfo <- withBuffer0' bufInfoB
            printMsg $ showBufInfo bufInfo
         where
         showBufInfo :: BufferFileInfo -> String
         showBufInfo bufInfo = concat [ show $ bufInfoFileName bufInfo
              , " Line "
              , show $ bufInfoLineNo bufInfo
              , " ["
              , bufInfoPercent bufInfo
              , "]"
              ]


-- | write the current buffer, but only if modified (cf. :help :x)
viWriteModified :: YiM ()
viWriteModified = do unchanged <- withBuffer' isUnchangedB
                     unless unchanged viWrite

viFnewE :: String -> YiM ()
viFnewE = fnewE . dropSpace

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
leave = oneOf [spec KEsc, ctrlCh 'c'] >> adjustPriority (-1) >> write clrStatus

leaveInsRep :: VimMode
leaveInsRep = do oneOf [spec KEsc, ctrlCh '[', ctrlCh 'c']
                 adjustPriority (-1)
                 write $ withBuffer0 (setMarkHere '^') >> clrStatus


-- | Insert mode is either insertion actions, or the meta (\ESC) action
-- TODO repeat
ins_mode :: ModeMap -> VimMode
ins_mode self = write (setStatus ("-- INSERT --", defaultStyle)) >> many (v_ins_char self <|> kwd_mode) >> leaveInsRep >> write (moveXorSol 1)

beginIns :: (Show x, YiAction a x) => ModeMap -> a -> I Event Action ()
beginIns self a = write a >> ins_mode self

post :: Monad m => m a -> m () -> m a
f `post` g = do x <- f
                g
                return x

withBuffer0' :: BufferM a -> EditorM a
withBuffer0' f = withBuffer0 $ f `post` leftOnEol

withBuffer' :: BufferM a -> YiM a
withBuffer' = withEditor . withBuffer0'

withEditor' :: EditorM a -> YiM a
withEditor' f = withEditor $ f `post` withBuffer0 leftOnEol

-- Find the item after or under the cursor and jump to its match
percentMove :: (RegionStyle, ViMove)
percentMove = (Inclusive, ArbMove tryGoingToMatch)
    where tryGoingToMatch = do
              p <- pointB
              foundMatch <- goToMatch
              when (not foundMatch) $ moveTo p
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
                         if eof || eol
                             then return False
                             else rightB >> goToMatch -- search for matchable character after the cursor

jumpToMark :: Char -> BufferM ()
jumpToMark c = do
  mm <- mayGetViMarkB c
  case mm of
    Nothing -> fail "Mark not set"
    Just m -> do
       p_next <- getMarkPointB m
       -- Retain the current point in the mark "'" automatically.
       p <- pointB
       getViMarkB '\'' >>= flip setMarkPointB p
       -- now jump to p_next.
       moveTo p_next

setMark :: VimMode
setMark = do
    c <- validMarkIdentifier
    write $ do
        p <- pointB
        -- Retain the current point in the mark "'" automatically.
        getViMarkB '\'' >>= flip setMarkPointB p
        getViMarkB c >>= flip setMarkPointB p

setMarkHere :: Char -> BufferM ()
setMarkHere c = do
    p <- pointB
    getViMarkB c >>= flip setMarkPointB p

getViMarkB :: Char -> BufferM Mark
getViMarkB '<' = selMark <$> askMarks
getViMarkB  c  = getMarkB $ Just [c]

mayGetViMarkB :: Char -> BufferM (Maybe Mark)
mayGetViMarkB '<' = Just . selMark <$> askMarks
mayGetViMarkB  c  = mayGetMarkB [c]

validMarkIdentifier :: (MonadInteract m w Event) => m Char 
validMarkIdentifier = fmap f $ oneOfchar "<>^'`" <|> charOf id 'a' 'z' <|> fail "Not a valid mark identifier."
  where oneOfchar = choice . map (\c -> event (char c) >> return c)
        f '`' = '\''
        f  c  =  c

-- --------------------
-- | Keyword
kwd_mode :: VimMode
kwd_mode = some (ctrlCh 'n' ?>> write wordComplete) >> deprioritize >> write resetComplete
-- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
-- and resetting it (restarting at the 1st completion).


