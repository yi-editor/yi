module Yi.Keymap.Vim2.NormalMap
  ( defNormalMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char
import Data.List (dropWhile)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Rope as R

import Yi.Buffer hiding (Insert)
import Yi.Core (quitEditor)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Eval
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

motionBinding :: VimBinding
motionBinding = VimBindingE prereq action
    where prereq ev state | vsMode state == Normal =
                                case ev of
                                    Event (KASCII c) [] -> isJust (stringToMove s)
                                        where s = dropWhile isDigit (vsAccumulator state) ++ [c]
                                    _ -> False
                          | otherwise = False
          action (Event (KASCII c) []) = do
              state <- getDynamic
              let s = dropWhile isDigit (vsAccumulator state) ++ [c]
                  (Just (Move _ move)) = stringToMove s
              count <- getCountE
              withBuffer0 $ move count >> leftOnEol
              resetCountE

              -- moving with j/k after $ sticks cursor to the right edge
              when (c == '$') $ setStickyEolE True
              when (c `elem` "jk" && vsStickyEol state) $ withBuffer0 $ moveToEol >> leftB
              when (c `notElem` "jk$") $ setStickyEolE False

              return Drop

zeroBinding :: VimBinding
zeroBinding = VimBindingE prereq action
    where prereq ev state = ev == char '0' && (vsMode state == Normal)
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
    where prereq ev state = ev == char '.' && (vsMode state == Normal)
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
        Just (Register style rope) -> withBuffer0 $ do
            insertRopeWithStyleB rope style
            leftB

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
            insertRopeWithStyleB rope style
            leftB

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
    , (char 'y', return (), id) -- TODO
    , (char 'Y', return (), id) -- TODO

    -- Search
    , (char '/', return (), id) -- TODO
    , (char '?', return (), id) -- TODO
    , (char '*', return (), id) -- TODO
    , (char '#', return (), id) -- TODO
    , (char 'n', return (), id) -- TODO
    , (char 'N', return (), id) -- TODO
    , (char 'f', return (), switchMode (NormalGotoCharacter Forward Inclusive))
    , (char 'F', return (), switchMode (NormalGotoCharacter Backward Inclusive))
    , (char 't', return (), switchMode (NormalGotoCharacter Forward Exclusive))
    , (char 'T', return (), switchMode (NormalGotoCharacter Backward Exclusive))
    , (char ';', return (), id) -- TODO

    -- Transition to visual
    -- , (char 'v', return (), switchMode (Visual Inclusive))
    -- , (char 'V', return (), switchMode (Visual LineWise))
    -- , (ctrlCh 'v', return (), switchMode (Visual Block))
    , (char 'v', return (), id)
    , (char 'V', return (), id)
    , (ctrlCh 'v', return (), id)

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
    , (char ',', return (), id)
    , (spec KEnter, return (), id)
    ]

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
