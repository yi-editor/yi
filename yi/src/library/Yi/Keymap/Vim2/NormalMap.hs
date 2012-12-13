module Yi.Keymap.Vim2.NormalMap
  ( defNormalMap
  ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Rope as R

import Yi.Buffer hiding (Insert)
import Yi.Core (quitEditor)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Eval
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.Utils

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = mkBindingE Normal Continue (char c, return (), mutate)
    where mutate vs@(VimState {vsCount = Nothing}) = vs { vsCount = Just d }
          mutate vs@(VimState {vsCount = Just count}) = vs { vsCount = Just $ count * 10 + d }
          d = ord c - ord '0'

defNormalMap :: [VimBinding]
defNormalMap = mkBindingY Normal (spec (KFun 10), quitEditor, id) : pureBindings

pureBindings :: [VimBinding]
pureBindings =
    [zeroBinding, repeatBinding, motionBinding] ++
    fmap mkDigitBinding ['1' .. '9'] ++
    finishingBingings ++
    continuingBindings ++
    nonrepeatableBindings

motionBinding = mkMotionBinding $ \m -> case m of
                                     Normal -> True
                                     _ -> False
-- motionBinding :: VimBinding
-- motionBinding = VimBindingE prereq action
--     where prereq e (VimState { vsMode = Normal, vsBindingAccumulator = bacc }) =
--               let s = bacc ++ eventToString e
--               in fmap (const ()) (stringToMove s)
--           prereq _ _ = NoMatch
--           action e = do
--               state <- getDynamic
--               let s = vsBindingAccumulator state ++ estring
--                   estring = eventToString e
--                   WholeMatch (Move _style move) = stringToMove s
--               count <- getMaybeCountE
--               withBuffer0 $ move count >> leftOnEol
--               resetCountE
-- 
--               -- moving with j/k after $ sticks cursor to the right edge
--               when (estring == "$") $ setStickyEolE True
--               when (estring `elem` ["j", "k"] && vsStickyEol state) $
--                   withBuffer0 $ moveToEol >> leftB
--               when (estring `notElem` ["j", "k", "$"]) $ setStickyEolE False
-- 
--               return Drop

zeroBinding :: VimBinding
zeroBinding = VimBindingE prereq action
    where prereq evs state = matchFromBool $ evs == "0" && (vsMode state == Normal)
          action _ = do
              currentState <- getDynamic
              case vsCount currentState of
                  Just c -> do
                      setDynamic $ currentState { vsCount = Just (10 * c) }
                      return Continue
                  Nothing -> do
                      withBuffer0 moveToSol
                      setDynamic $ resetCount currentState
                      return Drop

repeatBinding :: VimBinding
repeatBinding = VimBindingE prereq action
    where prereq evs state = matchFromBool $ evs == "." && (vsMode state == Normal)
          action _ = do
                currentState <- getDynamic
                case vsRepeatableAction currentState of
                    Nothing -> return ()
                    Just (RepeatableAction prevCount actionString) -> do
                        let count = fromMaybe prevCount (vsCount currentState)
                        scheduleActionStringForEval $ show count ++ actionString
                        resetCountE
                return Drop

finishingBingings :: [VimBinding]
finishingBingings = fmap (mkBindingE Normal Finish)
    [ (char 'x', cutCharE Forward =<< getCountE, resetCount)
    , (char 'X', cutCharE Backward =<< getCountE, resetCount)

    -- Pasting
    , (char 'p', pasteAfter, id)
    , (char 'P', pasteBefore, id)
    ]

pasteBefore :: EditorM ()
pasteBefore = do
    -- TODO: use count
    s <- getDefaultRegisterE
    case s of
        Nothing -> return ()
        Just (Register LineWise rope) -> withBuffer0 $ when (not $ R.null rope) $ do
            -- Beware of edge cases ahead
            let l = R.length rope
                lastChar = head $ R.toString $ R.drop (l-1) rope
                rope' = if lastChar == '\n' then rope else R.append rope (R.fromString "\n")
            insertRopeWithStyleB rope' LineWise
        Just (Register style rope) -> withBuffer0 $ pasteInclusiveB rope style

pasteAfter :: EditorM ()
pasteAfter = do
    -- TODO: use count
    register <- getDefaultRegisterE
    case register of
        Nothing -> return ()
        Just (Register LineWise rope) -> withBuffer0 $ do
            -- Beware of edge cases ahead
            moveToEol
            eof <- atEof
            when eof $ insertB '\n'
            rightB
            insertRopeWithStyleB rope LineWise
            when eof $ savingPointB $ do
                newSize <- sizeB
                moveTo (newSize - 1)
                curChar <- readB
                when (curChar == '\n') $ deleteN 1
        Just (Register style rope) -> withBuffer0 $ do
            rightB
            pasteInclusiveB rope style

pasteInclusiveB :: Rope -> RegionStyle -> BufferM ()
pasteInclusiveB rope style = do
    p0 <- pointB
    insertRopeWithStyleB rope style
    if R.countNewLines rope == 0 && style `elem` [Exclusive, Inclusive]
    then leftB
    else moveTo p0

continuingBindings :: [VimBinding]
continuingBindings = fmap (mkBindingE Normal Continue)
    [ (char 'r', return (), switchMode ReplaceSingleChar)
    , (char 'd', return (), switchMode (NormalOperatorPending OpDelete))
    , (char 'y', return (), switchMode (NormalOperatorPending OpYank))
    , (char 'c', return (), switchMode (NormalOperatorPending OpChange))
    , (char '=', return (), switchMode (NormalOperatorPending OpReindent))
    , (char '<', return (), switchMode (NormalOperatorPending OpShiftRight))
    , (char '>', return (), switchMode (NormalOperatorPending OpShiftLeft))

    -- Transition to insert mode
    , (char 'i', return (), switchMode Insert)
    , (char 'I', withBuffer0 firstNonSpaceB, switchMode Insert)
    , (char 'a', withBuffer0 rightB, switchMode Insert)
    , (char 'A', withBuffer0 moveToEol, switchMode Insert)
    , (char 'o', withBuffer0 $ do
                     moveToEol
                     insertB '\n'
        , switchMode Insert)
    , (char 'O', withBuffer0 $ do
                     moveToSol
                     insertB '\n'
                     leftB
        , switchMode Insert)

    -- Transition to visual
    , (char 'v', enableVisualE Inclusive, resetCount . switchMode (Visual Inclusive))
    , (char 'V', enableVisualE LineWise, resetCount . switchMode (Visual LineWise))
    , (ctrlCh 'v', enableVisualE Block, resetCount . switchMode (Visual Block))
    ]
    ++ fmap (mkStringBindingE Normal Continue)
    [ ("gu", return (), switchMode (NormalOperatorPending OpLowerCase))
    , ("gU", return (), switchMode (NormalOperatorPending OpUpperCase))
    , ("g~", return (), switchMode (NormalOperatorPending OpSwitchCase))
    , ("gq", return (), switchMode (NormalOperatorPending OpFormat))
    , ("g?", return (), switchMode (NormalOperatorPending OpRot13))
    ]

nonrepeatableBindings :: [VimBinding]
nonrepeatableBindings = fmap (mkBindingE Normal Drop)
    [ (spec KEsc, return (), resetCount)
    , (ctrlCh 'c', return (), resetCount)

    -- Changing
    , (char 'c', return (), switchMode Insert) -- TODO
    , (char 'C', do
        region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
        applyOperatorToRegionE OpDelete $ StyledRegion Exclusive region
        return (), switchMode Insert) -- TODO
    , (char 's', cutCharE Forward =<< getCountE, switchMode Insert)
    , (char 'S', do
        region <- withBuffer0 $ regionWithTwoMovesB firstNonSpaceB moveToEol
        applyOperatorToRegionE OpDelete $ StyledRegion Exclusive region
        , switchMode Insert)

    -- Replacing
    , (char 'R', return (), switchMode Replace)

    -- Deletion
    , (char 'D', return (), id) -- TODO

    -- Yanking
    , (char 'y', return (), switchMode $ NormalOperatorPending OpYank) -- TODO
    , (char 'Y', do
        region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
        applyOperatorToRegionE OpYank $ StyledRegion Exclusive region
        , id) -- TODO

    -- Search
    , (char '/', return (), id) -- TODO
    , (char '?', return (), id) -- TODO
    , (char '*', return (), id) -- TODO
    , (char '#', return (), id) -- TODO
    , (char 'n', return (), id) -- TODO
    , (char 'N', return (), id) -- TODO
    -- , (char 'f', return (), switchMode (NormalGotoCharacter Forward Inclusive))
    -- , (char 'F', return (), switchMode (NormalGotoCharacter Backward Inclusive))
    -- , (char 't', return (), switchMode (NormalGotoCharacter Forward Exclusive))
    -- , (char 'T', return (), switchMode (NormalGotoCharacter Backward Exclusive))
    , (char ';', repeatGotoCharE id, id) -- TODO
    , (char ',', repeatGotoCharE reverseDir, id) -- TODO

    -- Repeat
    , (char '&', return (), id) -- TODO

    -- Transition to ex
    , (char ':', return (), id) -- TODO

    -- Undo
    , (char 'u', return (), id) -- TODO
    , (char 'U', return (), id) -- TODO
    , (ctrlCh 'r', return (), id) -- TODO

    -- Indenting
    , (char '<', return (), id) -- TODO
    , (char '>', return (), id) -- TODO

    -- unsorted TODO
    , (char 'g', return (), id)
    , (char 'G', return (), id)
    , (char 'm', return (), id)
    , (char '[', return (), id)
    , (char ']', return (), id)
    , (char '{', return (), id)
    , (char '}', return (), id)
    , (char '-', return (), id)
    , (char '+', return (), id)
    , (char '~', return (), id)
    , (char '"', return (), id)
    , (char 'q', return (), id)
    , (spec KEnter, return (), id)
    ]

repeatGotoCharE :: (Direction -> Direction) -> EditorM ()
repeatGotoCharE mutateDir = do
    prevCommand <- fmap vsLastGotoCharCommand getDynamic
    count <- getCountE
    withBuffer0 $ case prevCommand of
        Just (GotoCharCommand c dir style) -> do
            let newDir = mutateDir dir
            let move = gotoCharacterB c newDir style True
            p0 <- pointB
            replicateM_ (count - 1) $ do
                move
                when (style == Exclusive) $ moveB Character newDir
            p1 <- pointB
            move
            p2 <- pointB
            when (p1 == p2) $ moveTo p0
        Nothing -> return ()

enableVisualE :: RegionStyle -> EditorM ()
enableVisualE style = withBuffer0 $ do
    putA regionStyleA style
    putA rectangleSelectionA $ Block == style
    setVisibleSelection True
    pointB >>= setSelectionMarkPointB

cutCharE :: Direction -> Int -> EditorM ()
cutCharE dir count = do
    r <- withBuffer0 $ do
        p0 <- pointB
        (if dir == Forward then moveXorEol else moveXorSol) count
        p1 <- pointB
        let region = mkRegion p0 p1
        rope <- readRegionB' region
        deleteRegionB $ mkRegion p0 p1
        leftOnEol
        return rope
    setDefaultRegisterE Inclusive r
