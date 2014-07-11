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

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.List (group)
import Data.Maybe (maybe)
import qualified Data.Rope as R
import Safe (headDef)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Motion
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.EventUtils
import Yi.Monad

-- 'mkBindingE' and 'mkBindingY' are helper functions for bindings
-- where VimState mutation is not dependent on action performed
-- and prerequisite has form (mode == ... && event == ...)

mkStringBindingE :: VimMode -> RepeatToken
    -> (String, EditorM (), VimState -> VimState) -> VimBinding
mkStringBindingE mode rtoken (eventString, action, mutate) = VimBindingE f
    where f _ vs | vsMode vs /= mode = NoMatch
          f evs _ = combineAction action mutate rtoken <$
                    evs `matchesString` eventString

mkStringBindingY :: VimMode
    -> (String, YiM (), VimState -> VimState) -> VimBinding
mkStringBindingY mode (eventString, action, mutate) = VimBindingY f
    where f _ vs | vsMode vs /= mode = NoMatch
          f evs _ = combineAction action mutate Drop <$
                    evs `matchesString` eventString

mkBindingE :: VimMode -> RepeatToken -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode rtoken (event, action, mutate) = VimBindingE f
    where f evs vs = combineAction action mutate rtoken <$
                     matchFromBool (vsMode vs == mode && evs == eventToString event)

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY f
    where f evs vs = combineAction action mutate Drop <$
                     matchFromBool (vsMode vs == mode && evs == eventToString event)

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

selectBinding :: String -> VimState -> [VimBinding] -> MatchResult (YiM RepeatToken)
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
    withBuffer0 $ setUnjumpMarks p
    addJumpAtE p

addVimJumpHereE :: EditorM ()
addVimJumpHereE = do
    withBuffer0 $ setUnjumpMarks =<< pointB
    addJumpHereE

mkMotionBinding :: RepeatToken -> (VimMode -> Bool) -> VimBinding
mkMotionBinding token condition = VimBindingE f
    where f evs state | condition (vsMode state) = fmap (go evs) (stringToMove evs)
          f _ _ = NoMatch
          go evs (Move _style isJump move) = do
              state <- getDynamic
              count <- getMaybeCountE
              prevPoint <- withBuffer0 $ do
                  p <- pointB
                  move count
                  leftOnEol
                  return p
              when isJump $ addVimJumpAtE prevPoint
              resetCountE

              -- moving with j/k after $ sticks cursor to the right edge
              when (evs == "$") $ setStickyEolE True
              when (evs `elem` group "jk" && vsStickyEol state) $
                  withBuffer0 $ moveToEol >> moveXorSol 1
              when (evs `notElem` group "jk$") $ setStickyEolE False

              let m = head evs
              when (m `elem` "fFtT") $ do
                  let c = last evs
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
mkChooseRegisterBinding statePredicate = VimBindingE f
    where f "\"" s | statePredicate s = PartialMatch
          f ['"', c] s | statePredicate s = WholeMatch $ do
              modifyStateE $ \s -> s { vsActiveRegister = c }
              return Continue
          f _ _ = NoMatch

indentBlockRegionB :: Int -> Region -> BufferM ()
indentBlockRegionB count reg = do
    indentSettings <- indentSettingsB
    (start, lengths) <- shapeOfBlockRegionB reg
    moveTo start
    forM_ (zip [1..] lengths) $ \(i, _) ->
        whenM (not <$> atEol) $ do
            if count > 0
            then insertN $ replicate (count * shiftWidth indentSettings) ' '
            else do
                let go 0 = return ()
                    go n = do
                        c <- readB
                        when (c == ' ') $
                            deleteN 1 >> go (n - 1)
                go (abs count * shiftWidth indentSettings)
            moveTo start
            void $ lineMoveRel i
    moveTo start

pasteInclusiveB :: R.Rope -> RegionStyle -> BufferM ()
pasteInclusiveB rope style = do
    p0 <- pointB
    insertRopeWithStyleB rope style
    if R.countNewLines rope == 0 && style `elem` [Exclusive, Inclusive]
    then leftB
    else moveTo p0

addNewLineIfNecessary :: R.Rope -> R.Rope
addNewLineIfNecessary rope = if lastChar == '\n'
                             then rope
                             else R.append rope (R.fromString "\n")
    where lastChar = head $ R.toString $ R.drop (R.length rope - 1) rope
