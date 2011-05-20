{-# LANGUAGE RelaxedPolyRec, FlexibleContexts, DeriveDataTypeable, TemplateHaskell, CPP, PatternGuards, GeneralizedNewtypeDeriving, FlexibleInstances #-}

-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008 Nicolas Pouillard

-- | Vim keymap for Yi. Emulates vim :set nocompatible
module Yi.Keymap.Vim (keymapSet, 
                      viWrite, 
                      defKeymap, 
                      leaveInsRep,
                      leave,
                      ModeMap(..),
                      VimOpts(..),
                      VimExCmd(..),
                      nilCmd,
                      exCmd,
                      exCmds,
                      exSimpleComplete,
                      exInfixComplete', exInfixComplete,
                      mkExHistComplete,
                      exHistComplete', exHistComplete,
                      exHistInfixComplete', exHistInfixComplete,
                      savingInsertB,
                      savingInsertCharB,
                      savingInsertStringB,
                      savingDeleteB,
                      savingDeleteCharB,
                      savingDeleteWordB,
                      savingCommandY,
                      savingCommandE,
                      mkKeymap,
                      beginIns,
                      beginInsE,
                      beginInsB,
                      listTagStack,
                      pushTagStack,
                      popTagStack,
                      peekTagStack
                      ) where

import Prelude (maybe, length, filter, map, drop, break, uncurry, reads)
import Yi.Prelude

import Data.Binary
import Data.Char
import Data.List (nub, take, words, dropWhile, takeWhile, intersperse, reverse)
import Data.Maybe (fromMaybe, isJust)
import Data.Either (either)
import Data.Prototype
import Data.Accessor.Template
import Numeric (showHex, showOct)
import Shim.Utils (splitBy, uncurry3)
import System.IO (readFile)
#ifdef mingw32_HOST_OS
import System.PosixCompat.Files (fileExist)
#else
import System.Posix (fileExist)
#endif
import System.FilePath (FilePath)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

import Control.Monad.State hiding (mapM_, mapM, sequence)
import Control.Arrow hiding (left, right)

import {-# source #-} Yi.Boot
import Yi.Command (cabalRun, makeBuild, shellCommandV)
import Yi.Core
import Yi.Eval (execEditorAction, getAllNamesInScope)
import Yi.File
import Yi.History
import Yi.Misc (matchingFileNames,adjBlock,adjIndent)
import Yi.String (dropSpace,lines')
import Yi.MiniBuffer
import Yi.Regex (seInput, regexEscapeString)
import Yi.Search
import Yi.Style
import Yi.TextCompletion
import Yi.Completion (containsMatch', mkIsPrefixOf)
import Yi.Tag 
import Yi.Window (bufkey)
import Yi.Hoogle (hoogle, hoogleSearch)
import qualified Codec.Binary.UTF8.String as UTF8


--
-- What's missing?
--   gq,gw,fillText should leave the last \n as is.
--   fancier :s// ==> missing /c, ...
--   '.': started look at "TODO repeat" for missing things
--   @:
--   8g8
--   provide and improve "g@{motion}"
--   integrate unit transposing (transposeB) (gSaw: swap a word)
--   free keys g[bBcClLnNOSWxXyYzZ(){}[/=|\:">.] z[BIJKpPqQSTVyYZ~`!@#$%&*()[]{}/?_:;,<>"'|\]
--   could be reused g[dDhHiQ]
--   free insert keys C-!, C-#, C-$, C-%, C-&, C-*, C-(, C-), C-/, C-+, C-=, C--, C-:, C-;, C-|
--   could be reused insert keys C-_, C-^
--   go
--   gm, g$, g<End>, gp, gP, ]p, ]P, [P, [p, gr, gR, gs, gv, gV
--   &, :&&, g&
--   goto file [{visual}][count]g{f,F}
--   invent a variant of o and O that add spaces to be at the same col
--   invent a "indent as previous" in insert mode
--   :sh[ell]
--   :!!
--   movement parameterised \> \<
--   motion operators [motion.txt]: !
--   C-v: visual block mode: almost works, block yanking/pasting is still to do.
--   Support for marks
--   C-o and C-i: jump list
--   C-a or C-@: insert prev text
--   C-u: delete all entered chars of the current line
--   C-f: reindent the line
--   C-o: execute one command, return to Insert mode (see also C-\ C-O)
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

data ViCmd = ArbCmd !(Int -> YiM ()) !Int
           | NoOp
  deriving (Typeable)

instance Binary ViCmd where
    put = dummyPut
    get = dummyGet

instance Initializable ViCmd where
  initial = NoOp
instance YiVariable ViCmd

data ViInsertion = ViIns { viActFirst  :: Maybe (EditorM ()) -- ^ The action performed first
                         , viActBefore :: BufferM () -- ^ The action performed before insertion
                         , viBeginPos  :: Point      -- ^ The position _before_ insertion
                         , viEndPos    :: Point      -- ^ The position _after_ insertion
                         , viActAfter  :: BufferM () -- ^ The action performed after insertion
                         }
  deriving (Typeable)

newtype MViInsertion = MVI { unMVI :: Maybe ViInsertion }
  deriving(Typeable, Initializable)

instance Binary MViInsertion where
    put = dummyPut
    get = dummyGet
instance YiVariable MViInsertion

$(nameDeriveAccessors ''ViInsertion $ Just.(++ "A"))
$(nameDeriveAccessors ''MViInsertion $ Just.(++ "_A"))

data VimOpts = VimOpts { tildeop :: Bool
                       , completeCaseSensitive :: Bool
                       , enableTagStack :: Bool
                       }

data VimExCmd = VimExCmd { cmdNames :: [String]
                         , cmdFn :: String -> YiM ()
                         , completeFn :: Maybe (String -> YiM ())
                         }

type VimExCmdMap = [VimExCmd] -- very simple implementation yet

newtype VimTagStack = VimTagStack { tagsStack :: [(FilePath, Point)] }
    deriving (Typeable, Binary)

instance Initializable VimTagStack where
    initial = VimTagStack []

instance YiVariable VimTagStack

getTagStack :: EditorM VimTagStack
getTagStack = getDynamic

setTagStack :: VimTagStack -> EditorM ()
setTagStack = setDynamic

listTagStack :: EditorM [(FilePath, Point)]
listTagStack = return . tagsStack =<< getTagStack

pushTagStack :: FilePath -> Point -> EditorM ()
pushTagStack fp p = do VimTagStack ts <- getTagStack
                       setTagStack $ VimTagStack $ (fp, p):ts

peekTagStack :: EditorM (Maybe (FilePath, Point))
peekTagStack = do VimTagStack ts <- getTagStack
                  case ts of
                    []    -> return Nothing
                    (p:_) -> return $ Just p

-- pop 'count' element from the tag stack.
popTagStack :: Int -> EditorM (Maybe (FilePath, Point))
popTagStack count = do VimTagStack ts <- getTagStack
                       case drop (count - 1) ts of
                         []     -> return Nothing
                         (p:ps) -> do setTagStack $ VimTagStack ps
                                      return $ Just p

$(nameDeriveAccessors ''VimOpts $ Just.(++ "A"))

-- | The Vim keymap is divided into several parts, roughly corresponding
-- to the different modes of vi. Each mode is in turn broken up into
-- separate VimProcs for each phase of key input in that mode.

data ModeMap = ModeMap { -- | Top level mode
                         v_top_level :: VimMode

                         -- | vim insert mode
                       , v_ins_char :: VimMode

                       , v_opts :: VimOpts

                       , v_ex_cmds :: VimExCmdMap

                       }

$(nameDeriveAccessors ''ModeMap $ Just.(++ "A"))

lastViCommandA :: Accessor Editor ViCmd
lastViCommandA = dynA

currentViInsertionA :: Accessor FBuffer (Maybe ViInsertion)
currentViInsertionA = unMVI_A . bufferDynamicValueA

applyViCmd :: Maybe Int -> ViCmd -> YiM ()
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

emptyViIns :: Point -> ViInsertion
emptyViIns p = ViIns Nothing (return ()) p p (return ())

getViIns :: BufferM ViInsertion
getViIns = maybe def return =<< getA currentViInsertionA
  where def = do ins <- emptyViIns <$> pointB
                 putA currentViInsertionA $ Just ins
                 return ins

viInsText :: ViInsertion -> BufferM String
viInsText ins = readRegionB $ mkRegion (viBeginPos ins) (viEndPos ins)

-- | The given buffer action should be an insertion action.
savingInsertB :: BufferM () -> BufferM ()
savingInsertB action = do ins0 <- getViIns
                          oldP <- pointB
                          action
                          newP <- pointB
                          let endP   = viEndPos ins0
                              beginP = viBeginPos ins0
                              ins1 | endP == oldP                  = ins0 { viEndPos = newP }
                                   | oldP >= beginP && oldP < endP = ins0 { viEndPos = endP +~ (newP ~- oldP) }
                                   | otherwise                     = emptyViIns newP
                          putA currentViInsertionA $ Just ins1

savingInsertCharB :: Char -> BufferM ()
savingInsertCharB = savingInsertB . insertB

savingInsertStringB :: String -> BufferM ()
savingInsertStringB = savingInsertB . insertN

-- | The given action should be a deletion action.
-- The only well tested buffer actions are deleting one character,
-- or one word, forward or backward.
savingDeleteB :: BufferM () -> BufferM ()
savingDeleteB action = do
  ins0 <- getViIns
  oldP <- pointB
  s1   <- sizeB
  action
  s2   <- sizeB
  newP <- pointB
  let diff   = s2 ~- s1
      endP   = viEndPos ins0
      beginP = viBeginPos ins0
      shrinkEndPos = viEndPosA ^: (-~ diff)
      ins1 =
        if oldP >= beginP && oldP <= endP then
          if newP > endP then
            viActAfterA  ^: (>> action) $ ins0 { viEndPos = newP }
          else if newP < beginP then
            viActBeforeA ^: (>> action) $ shrinkEndPos $ ins0 { viBeginPos = newP }
          else shrinkEndPos ins0
        else if newP > oldP then viActAfterA  ^: (>> action) $ emptyViIns newP
                            else viActBeforeA ^: (>> action) $ emptyViIns newP

  putA currentViInsertionA $ Just ins1

savingDeleteCharB :: Direction -> BufferM ()
savingDeleteCharB dir = savingDeleteB (adjBlock (-1) >> deleteB Character dir)

savingDeleteWordB :: Direction -> BufferM ()
savingDeleteWordB dir = savingDeleteB $ deleteRegionB =<< regionOfPartNonEmptyB unitViWordOnLine dir

viCommandOfViInsertion :: ViInsertion -> BufferM ViCmd
viCommandOfViInsertion ins@(ViIns mayFirstAct before _ _ after) = do
  text <- viInsText ins
  return . flip ArbCmd 1 . fmap withEditor $ case mayFirstAct of
    Just firstAct -> \n->
      replicateM_ n firstAct >> withBuffer0' (before >> insertN text >> after)
    Nothing ->
      flip replicateM_ $ withBuffer0' $ before >> insertN text >> after

commitLastInsertionE :: EditorM ()
commitLastInsertionE = do mins <- withBuffer0 $ getA currentViInsertionA
                          withBuffer0 $ putA currentViInsertionA Nothing
                          putA lastViCommandA =<< maybe (return NoOp) (withBuffer0 . viCommandOfViInsertion) mins

savingCommandY :: (Int -> YiM ()) -> Int -> YiM ()
savingCommandY f i = putA lastViCommandA (ArbCmd f i) >> f i

savingCommandE :: (Int -> EditorM ()) -> Int -> EditorM ()
savingCommandE f i = putA lastViCommandA (ArbCmd (withEditor . f) i) >> f i

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
                          setMarkHere '\''
                          moveTo $ Point $ floor (fromIntegral max_p * f)
                          firstNonSpaceB

mkKeymap :: Proto ModeMap -> KeymapSet
mkKeymap p = KeymapSet
  { -- if the keymap "crashed" we restart here
    -- so we clear the status line to indicate whatever mode we were in
    -- has been left
    startTopKeymap = do
      write clrStatus
      write $ setInserting False
      write $ setVisibleSelection False
  , startInsertKeymap = do
      write clrStatus
      write $ setInserting True
      write $ setVisibleSelection False
      write $ setStatus (["-- INSERT --"], defaultStyle)
  , topKeymap = v_top_level v
  , insertKeymap = v_ins_char v
  } where v = extractValue p

keymapSet :: KeymapSet
keymapSet = mkKeymap defKeymap

nilCmd :: VimExCmd
nilCmd = VimExCmd { cmdNames   = []
                  , cmdFn      = (return . const ())
                  , completeFn = Nothing}

exCmd :: String -> (String -> YiM ()) -> Maybe (String -> YiM ()) -> VimExCmd
exCmd names fn cfn = VimExCmd { cmdNames   = splitBy isSpace names
                              , cmdFn      = fn
                              , completeFn = cfn }

exCmds :: [(String, String->YiM (), Maybe (String -> YiM ()))] -> VimExCmdMap
exCmds = map $ uncurry3 exCmd

ignoreExCmd :: String -> String
ignoreExCmd = dropWhile (isSpace) . dropWhile (not . isSpace)

exSimpleComplete :: (String -> YiM [String]) -> String -> YiM ()
exSimpleComplete compl s' = simpleComplete compl s >>=
                            withBuffer . insertN . drop (length s)
    where s = dropWhile isSpace s'

exInfixComplete' :: Bool -> (String -> YiM [String]) -> String -> YiM ()
exInfixComplete' caseSensitive compl s' = do 
    cs <- infixComplete' caseSensitive compl s
    when (not $ null cs)
         (withBuffer $ do
              leftN (length s)
              deleteToEol 
              insertN cs)
   where s = dropWhile isSpace s'

exInfixComplete :: (String -> YiM [String]) -> String -> YiM ()
exInfixComplete = exInfixComplete' True

mkExHistComplete :: (String -> String -> Bool) -> (String -> YiM [String]) -> String -> YiM ()
mkExHistComplete matchFn compl s =
    mkWordComplete (return s) compl (withEditor . printMsgs . tail) matchFn >>= 
    (withBuffer . (testDeleteB >> ) . insertN)
  where 
    testDeleteB = if null s then return () else deleteWordB
    deleteWordB = deleteUnitB unitSep Backward
    deleteUnitB unit dir = deleteRegionB =<< regionOfPartNonEmptyB unit dir

exHistComplete' :: Bool -> (String -> YiM [String]) -> String -> YiM ()
exHistComplete' caseSensitive = mkExHistComplete (mkIsPrefixOf caseSensitive)

exHistComplete :: (String -> YiM [String]) -> String -> YiM ()
exHistComplete = exHistComplete' True

exHistInfixComplete' :: Bool -> (String -> YiM [String]) -> String -> YiM ()
exHistInfixComplete' caseSensitive = mkExHistComplete match 
  where match x y = isJust $ containsMatch' caseSensitive x y

exHistInfixComplete :: (String -> YiM [String]) -> String -> YiM ()
exHistInfixComplete = exHistInfixComplete' True

defKeymap :: Proto ModeMap
defKeymap = Proto template
  where 
    template self = ModeMap { v_top_level = def_top_level
                            , v_ins_char  = def_ins_char
                            , v_opts      = def_opts
                            , v_ex_cmds   = [] }
     where

     def_opts = VimOpts { tildeop = False
                        , completeCaseSensitive = True
                        , enableTagStack = True }

     -- | Top level consists of simple commands that take a count arg,
     -- the replace cmd, which consumes one char of input, and commands
     -- that switch modes.
     def_top_level = choice [cmd_eval,cmd_move,cmd2other,cmd_op]

     -- | Replace mode is like insert, except it performs writes, not inserts
     -- TODO repeat
     rep_mode :: VimMode
     rep_mode = write (setStatus (["-- REPLACE --"], defaultStyle)) >> many rep_char >> leaveInsRep >> write (moveXorSol 1)

     -- | Reset the selection style to a character-wise mode 'Inclusive'.
     resetSelectStyle :: BufferM ()
     resetSelectStyle = putA regionStyleA Inclusive

     -- | Visual mode, similar to command mode
     vis_move :: VimMode
     vis_move = (moveKeymap >>= write . viMove . snd)
                <|> do cnt <- count
                       let i = fromMaybe 1 cnt
                       choice ([events evs >>! action i | (evs,action) <- visOrCmdFM ] ++
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
       write $ do withBuffer0' $ putA regionStyleA selStyle
                  setStatus ([msg selStyle], defaultStyle)
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
     viMoveToNthEol n = replicateM_ n $ maybeMoveB Line Forward

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
                  [char 'G' ?>> return (LineWise, ArbMove $ setMarkHere '\'' >> maybe (botB >> firstNonSpaceB) gotoFNS cnt)
                  ,pString "gg" >> return (LineWise, ArbMove $ setMarkHere '\'' >> gotoFNS (fromMaybe 0 cnt))
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
         ,(char 'w',    jumpF unitViWord)
         ,(char 'W',    jumpF unitViWORD)
         ,(char 'b',    jumpB unitViWord)
         ,(char 'B',    jumpB unitViWORD)
         ,(ctrl $ spec KLeft,    jumpB unitViWORD)
         ,(ctrl $ spec KRight,   jumpF unitViWORD)
          -- text
         ,(char '{',    Replicate $ Move unitEmacsParagraph Backward)
         ,(char '}',    Replicate $ Move unitEmacsParagraph Forward)
         ,(char '(',    Replicate $ Move unitSentence  Backward)
         ,(char ')',    Replicate $ Move unitSentence  Forward)
         ]
         where
             left  = Replicate $ CharMove Backward
             right = Replicate $ CharMove Forward
             sol   = Replicate viMoveToSol
             eol   = ArbMove . viMoveToNthEol
             jumpF = \unit -> Replicate $ GenMove unit (Backward,InsideBound) Forward
             jumpB = \unit -> Replicate $ Move unit Backward

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
          [events evs >>! action i   | (evs, action) <- visOrCmdFM ] ++
          [events evs >>! action cnt | (evs, action) <- scrollCmdFM ] ++
          [char 'r' ?>> do c <- textChar
                           write $ savingCommandB (savingPointB . writeN . flip replicate c) i
          ,char 'm' ?>> setMark
          ,char '.' ?>>! applyViCmd cnt =<< withEditor (getA lastViCommandA)]


     searchCurrentWord :: Direction -> EditorM ()
     searchCurrentWord dir = do
       w <- withBuffer0' $ readRegionB =<< regionOfNonEmptyB unitViWord
       viSearch (boundedPattern  w) [] dir
       where
         boundedPattern x = "\\<" ++ (regexEscapeString x) ++ "\\>"

     gotoTag :: Tag -> YiM ()
     gotoTag tag =
       visitTagTable $ \tagTable ->
         case lookupTag tag tagTable of
           Nothing -> fail $ "No tags containing " ++ tag
           Just (filename, line) -> do
             when (enableTagStack $ v_opts self)
                  viTagStackPushPos
             viFnewE filename
             withBuffer' $ gotoLn line
             return ()

     viTagStackPushPos :: YiM ()
     viTagStackPushPos = withEditor $ do bn <- withBuffer0 $ gets identString
                                         p  <- withBuffer0 pointB
                                         pushTagStack bn p

     gotoPrevTagMark :: Int -> YiM ()
     gotoPrevTagMark count = do
       lastP <- withEditor $ popTagStack count
       case lastP of
         Nothing      -> withEditor $ fail "bottom of tag stack"
         Just (fp, p) -> do viFnewE fp
                            withBuffer' $ moveTo p

     -- | Call continuation @act@ with the TagTable. Uses the global table
     -- and prompts the user if it doesn't exist
     visitTagTable :: (TagTable -> YiM ()) -> YiM ()
     visitTagTable act = do
       posTagTable <- withEditor getTags
       -- does the tagtable exist?
       case posTagTable of
         Just tagTable -> act tagTable
         Nothing -> do fps <- withEditor getTagsFileList  -- withBuffer0' $ tagsFileList <$> getDynamicB
                       efps <- io $ filterM fileExist fps
                       when (null efps) $ fail ("No existing tags file among: " ++ show fps)
                       tagTable <- io $ importTagTable (head efps)
                       withEditor $ setTags tagTable
                       act tagTable

     gotoTagCurrentWord :: YiM ()
     gotoTagCurrentWord = gotoTag =<< withEditor (withBuffer0' (readRegionB =<< regionOfNonEmptyB unitViWord))

     -- | Parse any character that can be inserted in the text.
     textChar :: KeymapM Char
     textChar = do
       Event (KASCII c) [] <- anyEvent
       return c

     continueSearching :: (Direction -> Direction) -> EditorM ()
     continueSearching fdir = do
       m <- getRegexE
       dir <- fdir <$> getA searchDirectionA 
       printMsg $ directionElim dir '?' '/' : maybe "" seInput m
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
         (modifyRegionClever $ skippingLast $
            concat . (skippingFirst $ map $ skippingNull ((' ':) . dropWhile isSpace)) . lines')

     concatLinesB :: Region -> BufferM ()
     concatLinesB = savingPointB . (modifyRegionClever $ skippingLast $ filter (/='\n'))

     onCurrentWord :: (String -> String) -> BufferM ()
     onCurrentWord f = savingPointB $ modifyRegionClever f =<< regionOfNonEmptyB unitViWord

     onNumberInString :: (Read a, Show a, Num a) => (a -> a) -> String -> String
     onNumberInString f s = case reads s2 of
         []          -> s
         (n, rest):_ -> s1 ++ show (f n) ++ rest
       where (s1,s2) = break isDigit s

     -- as cmdFM but these commands are also valid in visual mode
     visOrCmdFM :: [([Event], Int -> YiM ())]
     visOrCmdFM =
         [([ctrlCh 'l'],      const userForceRefresh)
         ,([ctrlCh 'z'],      const suspendEditor)
         ,([ctrlCh 't'],      gotoPrevTagMark)
         ,([ctrlCh ']'],      const gotoTagCurrentWord) -- TODO add support for 'count'
         ] ++
         (fmap.second.fmap) withEditor
         [([ctrlW, char 'c'], const tryCloseE)
         ,([ctrlW, char 'o'], const closeOtherE)
         ,([ctrlW, char 's'], const splitE)
         ,([ctrlW, char 'w'], nextWinE')
         ,([ctrlW, ctrlW],    nextWinE')
         ,([ctrlW, char 'W'], prevWinE')
         ,([ctrlW, char 'p'], prevWinE')

         -- these 4 commands should go to moveKeymap
         -- however moveKeymap is currently confined to BufferM
         ,([char 'n'],          const $ continueSearching id)
         ,([char 'N'],          const $ continueSearching reverseDir)
         ,([char '*'],          const $ searchCurrentWord Forward)
         ,([char '#'],          const $ searchCurrentWord Backward)

         -- since we don't have vertical splitting,
         -- these moving can be done using next/prev.
         ,([ctrlW,spec KDown],  nextWinE')
         ,([ctrlW,spec KUp],    prevWinE')
         ,([ctrlW,spec KRight], nextWinE')
         ,([ctrlW,spec KLeft],  prevWinE')
         ,([ctrlW,char 'k'],    prevWinE')
         ,([ctrlW,char 'j'],    nextWinE')    -- Same as the above pair, when you're a bit slow to release ctl.
         ,([ctrlW, ctrlCh 'k'], prevWinE')
         ,([ctrlW, ctrlCh 'j'], nextWinE')
         ,(map char "ga",       const viCharInfo)
         ,(map char "g8",       const viChar8Info)
         ,(map char "gt",       nextTabE')
         ,(map char "gT",       prevTabE')
         ]
       where nextWinE' = flip replicateM_ nextWinE
             prevWinE' = flip replicateM_ prevWinE
             nextTabE' = flip replicateM_ nextTabE
             prevTabE' = flip replicateM_ previousTabE

     -- | cmd mode commands
     -- An event specified paired with an action that may take an integer argument.
     -- Usually the integer argument is the number of times an action should be repeated.
     cmdFM :: [([Event], Int -> YiM ())]
     cmdFM =
         [([ctrlCh 'g'],    const $ withEditor viFileInfo)

         ,([ctrlCh '^'],    withEditor . alternateBufferE . (+ (-1)) )

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
         ,([spec KDel],     savingCommandE'Y $ cut Exclusive . (Replicate $ CharMove Forward))

         -- pasting
         ,([char 'p'],      savingCommandEY $ flip replicateM_ pasteAfter)
         ,([char 'P'],      savingCommandEY $ flip replicateM_ pasteBefore)

         ,(map char "ZZ",   const $ viWriteModified >> closeWindow)
         ,(map char "ZQ",   const closeWindow)
         ]
         ++
         [ ([char '~'],     savingCommandB'Y $
                              (flip mapRegionB switchCaseChar =<<) .
                              flip regionOfViMove Exclusive .
                              Replicate (CharMove Forward))
         | not $ tildeop $ v_opts self ]

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
       choice   [let s1 = prefix [c]
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
                 , (ti, id, '~', const $ viMapRegion switchCaseChar)
                 , (g_, id, 'u', const $ viMapRegion toLower)
                 , (g_, id, 'U', const $ viMapRegion toUpper)
                 , (g_, g_, '?', const $ viMapRegion rot13Char)
                 , (g_, g_, 'q', const $ nonBlockRegion "gq" (const $ withBuffer0' . fillRegion))
                 , (g_, g_, 'w', const $ nonBlockRegion "gw" (const $ withBuffer0' . savingPointB . fillRegion))
                 ]
        where g_ = ('g':)
              ti = if tildeop $ v_opts self then id else g_
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
       setRegE $ if regionStyle == LineWise then '\n':txt else txt
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
       setRegE $ if regionStyle == LineWise then '\n':txt else txt

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
                 char 'I'     ?>> beginInsB self firstNonSpaceB,
                 pString "gi"  >> beginInsB self (jumpToMark '^'),
                 pString "gI"  >> beginInsB self moveToSol,
                 char 'a'     ?>> beginInsB self $ moveXorEol 1,
                 char 'A'     ?>> beginInsB self moveToEol,
                 char 'o'     ?>> beginInsB self $ moveToEol >> insertB '\n',
                 char 'O'     ?>> beginInsB self $ moveToSol >> insertB '\n' >> lineUp,
                 char 'c'     ?>> changeCmds,

                 -- FIXME: those two should take int argument
                 char 'C'     ?>> change NoMove Exclusive viMoveToEol, -- alias of "c$"
                 char 'S'     ?>> change viMoveToSol LineWise viMoveToEol, -- alias of "cc" TODO update
                 char 's'     ?>> change NoMove Exclusive (CharMove Forward), -- non-linewise alias of "cl"
                 char '/'     ?>>! ex_mode "/",
                 char '?'     ?>>! ex_mode "?",
                 leave,
                 spec KIns    ?>> ins_mode self]

     -- TODO cw,cW,cc,c[ai]<unit> don't support counting
     changeCmds :: I Event Action ()
     changeCmds =
       adjustPriority (-1) >>
         ((char 'w' ?>> change NoMove Exclusive (GenMove unitViWord (Forward, OutsideBound) Forward)) <|>
          (char 'W' ?>> change NoMove Exclusive (GenMove unitViWORD (Forward, OutsideBound) Forward))) <|>
       (char 'c' ?>> change NoMove LineWise NoMove) <|>
       (uncurry (change NoMove) =<< moveKeymap) <|>
       (select_any_unit (cutRegion Exclusive) >> ins_mode self) -- this correct while the RegionStyle is not LineWise

     change :: ViMove -> RegionStyle -> ViMove -> I Event Action ()
     change preMove regionStyle move =
       beginInsE self $ do
         withBuffer0' $ viMove preMove
         cut regionStyle move
         when (regionStyle == LineWise) $ withBuffer0' $ insertB '\n' >> leftB -- TODO repeat (savingInsertCharB?)

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
     insertNumber insrepB =
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
             g = f . sequence

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
              ,spec KDel      ?>>! savingDeleteCharB Forward
              ,spec KEnter    ?>>! savingInsertCharB '\n'
              ,(ctrl $ spec KLeft)  ?>>! moveB unitViWORD Backward
              ,(ctrl $ spec KRight) ?>>! genMoveB unitViWORD (Backward,InsideBound) Forward
              ,ctrlCh 'j'     ?>>! savingInsertCharB '\n'
              ,ctrlCh 'm'     ?>>! savingInsertCharB '\r'
              ,spec KTab      ?>>! mapM_ insrepB =<< tabB
              ,ctrlCh 'i'     ?>>! mapM_ insrepB =<< tabB
              ,ctrlCh 'e'     ?>>! insrepB =<< savingPointB (lineDown >> readB)
              ,ctrlCh 'y'     ?>>! insrepB =<< savingPointB (lineUp >> readB)
              ,ctrlCh 't'     ?>>! savingCommandB (const $ savingPointB $ shiftIndentOfRegion 1 =<< regionOfB Line) 1 --TODO should not move the cursor
              ,ctrlCh 'd'     ?>>! savingCommandE (const $ withBuffer0' $ savingPointB dedentOrDeleteIndent) 1 -- IDEM
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
            choice [spec KBS   ?>>! savingDeleteCharB Backward
                   ,ctrlCh 'h' ?>>! savingDeleteCharB Backward
                   ,ctrlCh 'w' ?>>! savingDeleteWordB Backward
                   ]
            <|> ins_rep_char savingInsertCharB
            <|| (textChar >>= write . (adjBlock 1 >>) . savingInsertCharB)

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
        where replaceB c = do e <- atEol; if e then insertB c else writeB c -- savingInsertCharB ?

     -- ---------------------------------------------------------------------
     -- Ex mode. We also process regex searching mode here.
     --
     findUserCmd :: String -> Maybe VimExCmd
     findUserCmd cmdLine = find ((name `elem`) . cmdNames) $ v_ex_cmds self
       where name = takeWhile (not . isSpace) $ dropWhile isSpace cmdLine

     ex_mode :: String -> EditorM ()
     ex_mode prompt = do
       -- The above ensures that the action is performed on the buffer that originated the minibuffer.
       let ex_buffer_finish = do
             withEditor historyFinish
             lineString <- withBuffer' elemsB
             withEditor closeBufferAndWindowE
             ex_eval (head prompt : lineString)
           ex_process :: VimMode
           ex_process = (some (spec KTab ?>>! completeMinibuffer) >> deprioritize >>! resetComplete)
               <|| choice [spec KEnter ?>>! ex_buffer_finish
                          ,spec KEsc   ?>>! closeBufferAndWindowE
                          ,ctrlCh 'h'  ?>>! actionAndHistoryPrefix $ deleteB Character Backward
                          ,spec KBS    ?>>! deleteBkdOrClose
                          ,spec KDel   ?>>! actionAndHistoryPrefix $ deleteB Character Forward
                          ,ctrlCh 'p'  ?>>! historyUp
                          ,spec KUp    ?>>! historyUp
                          ,ctrlCh 'n'  ?>>! historyDown
                          ,spec KDown  ?>>! historyDown
                          ,spec KLeft  ?>>! moveXorSol 1
                          ,spec KRight ?>>! moveXorEol 1
                          ,ctrlCh 'w'  ?>>! actionAndHistoryPrefix $ deleteB unitWord Backward
                          ,ctrlCh 'u'  ?>>! moveToSol >> deleteToEol]
                  <|| (insertChar >>! setHistoryPrefix)
           actionAndHistoryPrefix act = do
             withBuffer0 $ act
             setHistoryPrefix
           setHistoryPrefix = do
             ls <- withEditor . withBuffer0 $ elemsB
             historyPrefixSet ls
           insertChar = textChar >>= write . insertB
           deleteBkdOrClose = do
             ls <- withBuffer0 elemsB
             if null ls then closeBufferAndWindowE
                        else actionAndHistoryPrefix $ deleteB Character Backward

           findUserComplFn s | Just ex_cmd <- findUserCmd s = completeFn ex_cmd
                             | otherwise                    = Nothing

           completeMinibuffer = do s <- withBuffer elemsB
                                   case findUserComplFn s of
                                     Just cmplFn -> cmplFn $ ignoreExCmd s
                                     Nothing     -> ex_complete s

           f_complete = exSimpleComplete (matchingFileNames Nothing)
           b_complete = exSimpleComplete matchingBufferNames
           ex_complete ('c':'d':' ':f)                         = f_complete f
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
           ex_complete ('y':'i':' ':s)                         = exSimpleComplete (const getAllNamesInScope) s
           ex_complete s                                       = catchAllComplete s

           userExCmds = concatMap (map (++ " ") . cmdNames) $ v_ex_cmds self

           catchAllComplete = exSimpleComplete $ const $ return $
                                (userExCmds ++) $
                                ("hoogle-word" :) $ ("hoogle-search" : )$ ("set ft=" :) $ ("set tags=" :) $ map (++ " ") $ words $
                                "e edit r read saveas saveas! tabe tabnew tabm b buffer bd bd! bdelete bdelete! " ++
                                "yi cabal nohlsearch cd pwd suspend stop undo redo redraw reload tag .! quit quitall " ++
                                "qall quit! quitall! qall! write wq wqall ascii xit exit next prev" ++
                                "$ split new ball h help"
           cabalComplete = exSimpleComplete $ const $ return cabalCmds
           cabalCmds = words "configure install list update upgrade fetch upload check sdist" ++
                       words "report build copy haddock clean hscolour register test help"
           completeModes = exSimpleComplete $ const getAllModeNames

       historyStart
       historyPrefixSet ""
       spawnMinibufferE prompt $ const ex_process
       return ()

     -- | eval an ex command to an YiM (), also appends to the ex history
     ex_eval :: String -> YiM ()
     ex_eval cmd =
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
               (_:src) -> evalCmd $ dropSpace src

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
             canClose <- gets isUnchangedBuffer
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
             bs <- mapM (\b -> (,) b <$> withEditor (withGivenBuffer0 b needsAWindowB)) =<< readEditor bufferStack
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
           bdelete  = whenUnchanged (withBuffer' $ gets isUnchangedBuffer) . withEditor . closeBufferE . dropSpace
           bdeleteNoW = withEditor . closeBufferE . dropSpace

           -- the help feature currently try to show available key bindings
           help = withEditor (printMsg . show =<< acceptedInputs)


           evalCmd cmdLine = case findUserCmd cmdLine of
                               Just ex_cmd -> cmdFn ex_cmd $ ignoreExCmd cmdLine
                               Nothing     -> fn cmdLine

           -- fn maps from the text entered on the command line to a YiM () implementing the 
           -- command.
           fn ""           = withEditor clrStatus

           fn s | all isDigit s = withBuffer' (setMarkHere '\'' >> gotoLn (read s) >> firstNonSpaceB)

           fn "w"          = viWrite
           fn ('w':' ':f)  = viSafeWriteTo $ dropSpace f
           fn ('w':'r':'i':'t':'e':' ':f)  = viSafeWriteTo $ dropSpace f
           fn ('w':'!':' ':f)  = viWriteTo $ dropSpace f
           fn ('w':'r':'i':'t':'e':'!':' ':f)  = viWriteTo $ dropSpace f
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
           fn "qal!"       = quitEditor
           fn "qall!"      = quitEditor
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
           fn ('s':'a':'v':'e':'a':'s':' ':f)     = let f' = dropSpace f in discard $ viSafeWriteTo f' >> editFile f'
           fn ('s':'a':'v':'e':'a':'s':'!':' ':f) = let f' = dropSpace f in discard $ viWriteTo f' >> editFile f'
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

           fn "reload"     = reload >> return ()    -- not in vim

           fn "redr"       = userForceRefresh
           fn "redraw"     = userForceRefresh

           fn "u"          = withBuffer' undoB
           fn "undo"       = withBuffer' undoB
           fn "only"       = withEditor closeOtherE
           fn "red"        = withBuffer' redoB
           fn "redo"       = withBuffer' redoB

           fn ('c':'d':' ':f) = io . setCurrentDirectory . dropSpace $ f
           fn "pwd"        = (io $ getCurrentDirectory) >>= withEditor . printMsg

           fn "sus"        = suspendEditor
           fn "suspend"    = suspendEditor
           fn "st"         = suspendEditor
           fn "stop"       = suspendEditor

           fn ('c':'a':'b':'a':'l':' ':s) = cabalRun s1 (const $ return ()) (CommandArguments $ words $ drop 1 s2) where (s1, s2) = break (==' ') s
           fn "make"       = makeBuild $ CommandArguments []
           fn ('m':'a':'k':'e':' ':s) = makeBuild (CommandArguments $ words s)
           fn ('!':s)         = shellCommandV s
           fn ('y':'i':' ':s) = execEditorAction $ dropSpace s

           fn "hoogle-word" = hoogle >> return ()
           fn "hoogle-search" = hoogleSearch
           fn "h"          = help
           fn "help"       = help
           fn "tabm"       = withEditor (moveTab Nothing)
           fn ('t':'a':'b':'m':' ':n) = withEditor (moveTab $ Just (read n))
           fn "tabnew"     = withEditor $ do
               newTabE
               newTempBufferE
               return ()
           fn ('t':'a':'b':'e':' ':f) = withEditor newTabE >> viFnewE f

           fn "ball"       = withEditor openAllBuffersE

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
         where showSeq showX xs s = foldr ($) s $ intersperse (showChar ' ') $ map showX xs

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
viWriteModified = do unchanged <- withBuffer' $ gets isUnchangedBuffer
                     unless unchanged viWrite

viFnewE :: String -> YiM ()
viFnewE f = discard (editFile $ dropSpace f)

-- | viSearch is a doSearch wrapper that print the search outcome.
-- TODO: consider merging with doSearch 
viSearch :: String -> [SearchOption] -> Direction -> EditorM ()
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
leaveInsRep = do
    oneOf [spec KEsc, ctrlCh '[', ctrlCh 'c']
    adjustPriority (-1)
    write $ commitLastInsertionE >> withBuffer0 (setMarkHere '^')
    startTopKeymap keymapSet

-- | Insert mode is either insertion actions, or the meta (\ESC) action
-- TODO repeat
ins_mode :: ModeMap -> VimMode
ins_mode self = do
    startInsertKeymap keymapSet
    many (v_ins_char self <|> kwd_mode (v_opts self))
    leaveInsRep
    write $ moveXorSol 1

-- TODO refactor with beginInsB and beginInsE
beginIns :: (Show x, YiAction a x) => ModeMap -> a -> I Event Action ()
beginIns self a = write a >> ins_mode self

beginInsB :: ModeMap -> BufferM () -> I Event Action ()
beginInsB self = beginInsE self . withBuffer0

beginInsE :: ModeMap -> EditorM () -> I Event Action ()
beginInsE self a = do
  write $ do a
             withBuffer0 $ do p <- pointB
                              putA currentViInsertionA $ Just $ viActFirstA ^= Just a $ emptyViIns p
  ins_mode self

withBuffer0' :: BufferM a -> EditorM a
withBuffer0' f = withBuffer0 (f <* leftOnEol)

withBuffer' :: BufferM a -> YiM a
withBuffer' = withEditor . withBuffer0'

withEditor' :: EditorM a -> YiM a
withEditor' f = withEditor (f <* withBuffer0 leftOnEol)

-- Find the item after or under the cursor and jump to its match
percentMove :: (RegionStyle, ViMove)
percentMove = (Inclusive, ArbMove tryGoingToMatch)
    where tryGoingToMatch = do
              p <- pointB
              getViMarkB '\'' >>= flip setMarkPointB p
              foundMatch <- goToMatch
              unless foundMatch $ moveTo p
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
kwd_mode :: VimOpts -> VimMode
kwd_mode opts = some (ctrlCh 'n' ?>> write . viWordComplete $ completeCaseSensitive opts) >> 
                deprioritize >> 
                write resetComplete
-- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
-- and resetting it (restarting at the 1st completion).
  where viWordComplete caseSensitive = 
          withEditor . withBuffer0 . (savingDeleteWordB Backward >>) . 
          savingInsertStringB =<< wordCompleteString' caseSensitive
