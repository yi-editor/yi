{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Utils
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils for the Vim keymap.

module Yi.Keymap.Vim.Utils
  ( mkBindingE
  , mkBindingY
  , mkStringBindingE
  , mkStringBindingY
  , splitCountedCommand
  , selectBinding
  , selectPureBinding
  , matchFromBool
  , mkMotionBinding
  , mkChooseRegisterBinding
  , pasteInclusiveB
  , addNewLineIfNecessary
  , indentBlockRegionB
  , addVimJumpHereE
  ) where

import           Control.Applicative      ((<$), (<$>))
import           Control.Lens             ((.=), use)
import           Control.Monad            (forM_, void, when)
import           Data.Char                (isSpace)
import           Data.Foldable            (asum)
import           Data.List                (group)
import qualified Data.Text                as T (unpack)
import           Safe                     (headDef)
import           Yi.Buffer.Adjusted       hiding (Insert)
import           Yi.Editor
import           Yi.Event                 (Event)
import           Yi.Keymap                (YiM)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.EventUtils (eventToEventString, splitCountedCommand)
import           Yi.Keymap.Vim.Motion     (Move (Move), stringToMove)
import           Yi.Keymap.Vim.StateUtils (getMaybeCountE, modifyStateE, resetCountE)
import           Yi.Monad                 (whenM)
import           Yi.Rope                  (YiString, countNewLines, last)
import qualified Yi.Rope                  as R (replicateChar, snoc)

-- 'mkBindingE' and 'mkBindingY' are helper functions for bindings
-- where VimState mutation is not dependent on action performed
-- and prerequisite has form (mode == ... && event == ...)

mkStringBindingE :: VimMode -> RepeatToken
    -> (EventString, EditorM (), VimState -> VimState) -> VimBinding
mkStringBindingE mode rtoken (eventString, action, mutate) = VimBindingE f
    where f _ vs | vsMode vs /= mode = NoMatch
          f evs _ = combineAction action mutate rtoken <$
                    evs `matchesString` eventString

mkStringBindingY :: VimMode
    -> (EventString, YiM (), VimState -> VimState) -> VimBinding
mkStringBindingY mode (eventString, action, mutate) = VimBindingY f
    where f _ vs | vsMode vs /= mode = NoMatch
          f evs _ = combineAction action mutate Drop <$
                    evs `matchesString` eventString

mkBindingE :: VimMode -> RepeatToken -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode rtoken (event, action, mutate) = VimBindingE f
    where f evs vs = combineAction action mutate rtoken <$
                     matchFromBool (vsMode vs == mode && evs == eventToEventString event)

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY f
    where f evs vs = combineAction action mutate Drop <$
                     matchFromBool (vsMode vs == mode && evs == eventToEventString event)

combineAction :: MonadEditor m => m () -> (VimState -> VimState) -> RepeatToken -> m RepeatToken
combineAction action mutateState rtoken = do
    action
    withEditor $ modifyStateE mutateState
    return rtoken

-- | All impure bindings will be ignored.
selectPureBinding :: EventString -> VimState -> [VimBinding] -> MatchResult (EditorM RepeatToken)
selectPureBinding evs state = asum . fmap try
    where try (VimBindingE matcher) = matcher evs state
          try (VimBindingY _) = NoMatch

selectBinding :: EventString -> VimState -> [VimBinding] -> MatchResult (YiM RepeatToken)
selectBinding input state = asum . fmap try
    where try (VimBindingY matcher) = matcher input state
          try (VimBindingE matcher) = fmap withEditor $ matcher input state

matchFromBool :: Bool -> MatchResult ()
matchFromBool b = if b then WholeMatch () else NoMatch

setUnjumpMarks :: Point -> BufferM ()
setUnjumpMarks p = do
    solP <- solPointB p
    lineStream <- indexedStreamB Forward solP
    let fstNonBlank =
            headDef solP [ p' | (p', ch) <- lineStream, not (isSpace ch) || ch == '\n' ]
    (.= p)           . markPointA =<< getMarkB (Just "`")
    (.= fstNonBlank) . markPointA =<< getMarkB (Just "'")

addVimJumpAtE :: Point -> EditorM ()
addVimJumpAtE p = do
    withCurrentBuffer $ setUnjumpMarks p
    addJumpAtE p

addVimJumpHereE :: EditorM ()
addVimJumpHereE = do
    withCurrentBuffer $ setUnjumpMarks =<< pointB
    addJumpHereE

mkMotionBinding :: RepeatToken -> (VimMode -> Bool) -> VimBinding
mkMotionBinding token condition = VimBindingE f
  where
    -- TODO: stringToMove and go both to EventString
    f :: EventString -> VimState -> MatchResult (EditorM RepeatToken)
    f evs state | condition (vsMode state) =
        fmap (go . T.unpack . _unEv $ evs) (stringToMove evs)
    f _ _ = NoMatch

    go :: String -> Move -> EditorM RepeatToken
    go evs (Move _style isJump move) = do
        count <- getMaybeCountE
        prevPoint <- withCurrentBuffer $ do
            p <- pointB
            move count
            leftOnEol
            return p
        when isJump $ addVimJumpAtE prevPoint
        resetCountE

        sticky <- withCurrentBuffer $ use stickyEolA

        -- moving with j/k after $ sticks cursor to the right edge
        when (evs == "$") . withCurrentBuffer $ stickyEolA .= True
        when (evs `elem` group "jk" && sticky) $
            withCurrentBuffer $ moveToEol >> moveXorSol 1
        when (evs `notElem` group "jk$") . withCurrentBuffer $ stickyEolA .= False

        let m = head evs
        when (m `elem` ('f' : "FtT")) $ do
            let c = Prelude.last evs
                (dir, style) =
                    case m of
                        'f' -> (Forward, Inclusive)
                        't' -> (Forward, Exclusive)
                        'F' -> (Backward, Inclusive)
                        'T' -> (Backward, Exclusive)
                        _ -> error "can't happen"
                command = GotoCharCommand c dir style
            modifyStateE $ \s -> s { vsLastGotoCharCommand = Just command}

        return token

mkChooseRegisterBinding :: (VimState -> Bool) -> VimBinding
mkChooseRegisterBinding statePredicate = VimBindingE (f . T.unpack . _unEv)
    where f "\"" s | statePredicate s = PartialMatch
          f ['"', c] s | statePredicate s = WholeMatch $ do
              modifyStateE $ \s' -> s' { vsActiveRegister = c }
              return Continue
          f _ _ = NoMatch

indentBlockRegionB :: Int -> Region -> BufferM ()
indentBlockRegionB count reg = do
  indentSettings <- indentSettingsB
  (start, lengths) <- shapeOfBlockRegionB reg
  moveTo start
  forM_ (zip [1..] lengths) $ \(i, _) -> do
      whenM (not <$> atEol) $ do
        let w = shiftWidth indentSettings
        if count > 0
        then insertN $ R.replicateChar (count * w) ' '
        else go (abs count * w)
      moveTo start
      void $ lineMoveRel i
  moveTo start
  where
      go 0 = return ()
      go n = do
          c <- readB
          when (c == ' ') $
              deleteN 1 >> go (n - 1)


pasteInclusiveB :: YiString -> RegionStyle -> BufferM ()
pasteInclusiveB rope style = do
  p0 <- pointB
  insertRopeWithStyleB rope style
  if countNewLines rope == 0 && style `elem` [ Exclusive, Inclusive ]
    then leftB
    else moveTo p0

trailingNewline :: YiString -> Bool
trailingNewline t = case Yi.Rope.last t of
  Just '\n' -> True
  _         -> False

addNewLineIfNecessary :: YiString -> YiString
addNewLineIfNecessary rope =
  if trailingNewline rope then rope else rope `R.snoc` '\n'
