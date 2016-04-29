{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Editor
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The top level editor state, and operations on it. This is inside an
-- internal module for easy re-export with Yi.Types bits.

module Yi.Editor ( Editor(..), EditorM, MonadEditor(..)
                 , runEditor
                 , acceptedInputsOtherWindow
                 , addJumpAtE
                 , addJumpHereE
                 , alternateBufferE
                 , askConfigVariableA
                 , bufferSet
                 , buffersA
                 , closeBufferAndWindowE
                 , closeBufferE
                 , closeOtherE
                 , clrStatus
                 , commonNamePrefix
                 , currentBuffer
                 , currentRegexA
                 , currentWindowA
                 , deleteBuffer
                 , deleteTabE
                 , emptyEditor
                 , findBuffer
                 , findBufferWith
                 , findBufferWithName
                 , findWindowWith
                 , focusWindowE
                 , getBufferStack
                 , getBufferWithName
                 , getBufferWithNameOrCurrent
                 , getEditorDyn
                 , getRegE
                 , jumpBackE
                 , jumpForwardE
                 , killringA
                 , layoutManagerNextVariantE
                 , layoutManagerPreviousVariantE
                 , layoutManagersNextE
                 , layoutManagersPreviousE
                 , maxStatusHeightA
                 , moveTabE
                 , moveWinNextE
                 , moveWinPrevE
                 , newBufferE
                 , newEmptyBufferE
                 , newTabE
                 , newTempBufferE
                 , newWindowE
                 , nextTabE
                 , nextWinE
                 , onCloseActionsA
                 , pendingEventsA
                 , prevWinE
                 , previousTabE
                 , printMsg
                 , printMsgs
                 , printStatus
                 , pushWinToFirstE
                 , putEditorDyn
                 , searchDirectionA
                 , setDividerPosE
                 , setRegE
                 , setStatus
                 , shiftOtherWindow
                 , splitE
                 , statusLine
                 , statusLineInfo
                 , statusLinesA
                 , stringToNewBuffer
                 , swapWinWithFirstE
                 , switchToBufferE
                 , switchToBufferWithNameE
                 , tabsA
                 , tryCloseE
                 , windows
                 , windowsA
                 , windowsOnBufferE
                 , withCurrentBuffer
                 , withEveryBuffer
                 , withGivenBuffer
                 , withGivenBufferAndWindow
                 , withOtherWindow
                 , withWindowE
                 ) where

import           Prelude                        hiding (all, concatMap, foldl, foldr)

import           Control.Applicative            ((<$>), (<*>))
import           Control.Lens                   (Lens', assign, lens, mapped,
                                                 use, uses, view, (%=), (%~),
                                                 (&), (.~), (^.))
import           Control.Monad                  (forM_, liftM)
import           Control.Monad.Reader           (MonadReader (ask), asks,
                                                 unless, when)
import           Control.Monad.State            (gets, modify)
import           Data.Binary                    (Binary, get, put)
import           Data.Default                   (Default, def)
import qualified Data.DelayList                 as DelayList (insert)
import           Data.DynamicState.Serializable (getDyn, putDyn)
import           Data.Foldable                  (Foldable (foldl, foldr), all, concatMap, toList)
import           Data.List                      (delete, (\\))
import           Data.List.NonEmpty             (NonEmpty (..), fromList, nub)
import qualified Data.List.NonEmpty             as NE (filter, head, length, toList, (<|))
import qualified Data.List.PointedList          as PL (atEnd, moveTo)
import qualified Data.List.PointedList.Circular as PL (PointedList (..), delete,
                                                       deleteLeft, deleteOthers,
                                                       deleteRight, focus,
                                                       insertLeft, insertRight,
                                                       length, next, previous,
                                                       singleton, _focus)
import qualified Data.Map                       as M (delete, elems, empty,
                                                      insert, lookup, singleton, (!))
import           Data.Maybe                     (fromJust, fromMaybe, isNothing)
import qualified Data.Monoid                    as Mon ((<>))
import           Data.Semigroup                 (mempty, (<>))
import qualified Data.Text                      as T (Text, null, pack, unlines, unpack, unwords)
import           System.FilePath                (splitPath)
import           Yi.Buffer
import           Yi.Config
import           Yi.Interact                    as I (accepted, mkAutomaton)
import           Yi.JumpList                    (Jump (..), JumpList, addJump, jumpBack, jumpForward)
import           Yi.KillRing                    (krEmpty, krGet, krPut, krSet)
import           Yi.Layout
import           Yi.Monad                       (getsAndModify)
import           Yi.Rope                        (YiString, empty, fromText)
import qualified Yi.Rope                        as R (YiString, fromText, snoc)
import           Yi.String                      (listify)
import           Yi.Style                       (defaultStyle)
import           Yi.Tab
import           Yi.Types
import           Yi.Utils
import           Yi.Window

instance Binary Editor where
  put (Editor bss bs supply ts dv _sl msh kr regex _dir _ev _cwa ) =
    let putNE (x :| xs) = put x >> put xs
    in putNE bss >> put bs >> put supply >> put ts
       >> put dv >> put msh >> put kr >> put regex
  get = do
    bss <- (:|) <$> get <*> get
    bs <- get
    supply <- get
    ts <- get
    dv <- get
    msh <- get
    kr <- get
    regex <- get
    return $ emptyEditor { bufferStack = bss
                         , buffers = bs
                         , refSupply = supply
                         , tabs_ = ts
                         , dynamic = dv
                         , maxStatusHeight = msh
                         , killring = kr
                         , currentRegex = regex
                         }

-- | The initial state
emptyEditor :: Editor
emptyEditor = Editor
  { buffers      = M.singleton (bkey buf) buf
  , tabs_        = PL.singleton tab
  , bufferStack  = bkey buf :| []
  , refSupply    = 3
  , currentRegex = Nothing
  , searchDirection = Forward
  , dynamic      = mempty
  , statusLines  = DelayList.insert (maxBound, ([""], defaultStyle)) []
  , killring     = krEmpty
  , pendingEvents = []
  , maxStatusHeight = 1
  , onCloseActions = M.empty
  }
  where buf = newB 0 (MemBuffer "console") mempty
        win = (dummyWindow (bkey buf)) { wkey = WindowRef 1 , isMini = False }
        tab = makeTab1 2 win


-- ---------------------------------------------------------------------

makeLensesWithSuffix "A" ''Editor

windows :: Editor -> PL.PointedList Window
windows e = e ^. windowsA

windowsA :: Lens' Editor (PL.PointedList Window)
windowsA = currentTabA . tabWindowsA

tabsA :: Lens' Editor (PL.PointedList Tab)
tabsA = fixCurrentBufferA_ . tabs_A

currentTabA :: Lens' Editor Tab
currentTabA = tabsA . PL.focus

askConfigVariableA :: (YiConfigVariable b, MonadEditor m) => m b
askConfigVariableA = do cfg <- askCfg
                        return $ cfg ^. configVariable

-- ---------------------------------------------------------------------
-- Buffer operations

newRef :: MonadEditor m => m Int
newRef = withEditor (refSupplyA %= (+ 1) >> use refSupplyA)

newBufRef :: MonadEditor m => m BufferRef
newBufRef = liftM BufferRef newRef

-- | Create and fill a new buffer, using contents of string.
-- | Does not focus the window, or make it the current window.
-- | Call newWindowE or switchToBufferE to take care of that.
stringToNewBuffer :: MonadEditor m
                  => BufferId -- ^ The buffer indentifier
                  -> YiString -- ^ The contents with which to populate
                              -- the buffer
                  -> m BufferRef
stringToNewBuffer nm cs = withEditor $ do
    u <- newBufRef
    defRegStyle <- configRegionStyle <$> askCfg
    insertBuffer $ newB u nm cs
    m <- asks configFundamentalMode
    withGivenBuffer u $ do
        putRegionStyle defRegStyle
        setAnyMode m
    return u

insertBuffer :: MonadEditor m => FBuffer -> m ()
insertBuffer b = withEditor . modify $ \e ->
    -- insert buffers at the end, so that
    -- "background" buffers do not interfere.
    e { bufferStack = nub (bufferStack e <> (bkey b :| []))
      , buffers = M.insert (bkey b) b (buffers e)}


-- Prevent possible space leaks in the editor structure
forceFold1 :: Foldable t => t a -> t a
forceFold1 x = foldr seq x x

forceFoldTabs :: Foldable t => t Tab -> t Tab
forceFoldTabs x = foldr (seq . forceTab) x x

-- | Delete a buffer (and release resources associated with it).
deleteBuffer :: MonadEditor m => BufferRef -> m ()
deleteBuffer k = withEditor $ do
  -- If the buffer has an associated close action execute that now.
  -- Unless the buffer is the last buffer in the editor. In which case
  -- it cannot be closed and, I think, the close action should not be
  -- applied.
  --
  -- The close actions seem dangerous, but I know of no other simple
  -- way to resolve issues related to what buffer receives actions
  -- after the minibuffer closes.
  gets bufferStack >>= \case
    _ :| [] -> return ()
    _ -> M.lookup k <$> gets onCloseActions
         >>= \m_action -> fromMaybe (return ()) m_action
  -- Now try deleting the buffer. Checking, once again, that it is not
  -- the last buffer.
  bs <- gets bufferStack
  ws <- use windowsA
  case bs of
    b0 :| nextB : _ -> do
      let pickOther w = if bufkey w == k then w {bufkey = other} else w
          visibleBuffers = bufkey <$> toList ws

          -- This ‘head’ always works because we witness that length of
          -- bs ≥ 2 (through case) and ‘delete’ only deletes up to 1
          -- element so we at worst we end up with something like
          -- ‘head $ [] ++ [foo]’ when bs ≡ visibleBuffers
          bs' = NE.toList bs
          other = head $ (bs' \\ visibleBuffers) ++ delete k bs'
      when (b0 == k) $
        -- we delete the currently selected buffer: the next buffer
        -- will become active in the main window, therefore it must be
        -- assigned a new window.
        switchToBufferE nextB

      -- NOTE: This *only* works if not all bufferStack buffers are
      -- equivalent to ‘k’. Assuring that there are no duplicates in
      -- the bufferStack is equivalent in this case because of its
      -- length.
      modify $ \e ->
        e & bufferStackA %~ fromList . forceFold1 . NE.filter (k /=)
          & buffersA %~ M.delete k
          & tabs_A %~ forceFoldTabs . fmap (mapWindows pickOther)
          -- all windows open on that buffer must switch to another
          -- buffer.

      windowsA . mapped . bufAccessListA %= forceFold1 . filter (k /=)
    _ -> return () -- Don't delete the last buffer.

-- | Return the buffers we have, /in no particular order/
bufferSet :: Editor -> [FBuffer]
bufferSet = M.elems . buffers

-- | Return a prefix that can be removed from all buffer paths while
-- keeping them unique.
commonNamePrefix :: Editor -> [FilePath]
commonNamePrefix = commonPrefix . fmap (dropLast . splitPath)
                   . fbufs . fmap (^. identA) . bufferSet
  where dropLast [] = []
        dropLast x = init x
        fbufs xs = [ x | FileBuffer x <- xs ]
          -- drop the last component, so that it is never hidden.

getBufferStack :: MonadEditor m => m (NonEmpty FBuffer)
getBufferStack = withEditor $ do
  bufMap <- gets buffers
  gets $ fmap (bufMap M.!) . bufferStack

findBuffer :: MonadEditor m => BufferRef -> m (Maybe FBuffer)
findBuffer k = withEditor (gets (M.lookup k . buffers))

-- | Find buffer with this key
findBufferWith :: BufferRef -> Editor -> FBuffer
findBufferWith k e = case M.lookup k (buffers e) of
  Just x -> x
  Nothing -> error "Editor.findBufferWith: no buffer has this key"

-- | Find buffers with this name
findBufferWithName :: T.Text -> Editor -> [BufferRef]
findBufferWithName n e =
  let bufs = M.elems $ buffers e
      sameIdent b = shortIdentString (length $ commonNamePrefix e) b == n
  in map bkey $ filter sameIdent bufs

-- | Find buffer with given name. Fail if not found.
getBufferWithName :: MonadEditor m => T.Text -> m BufferRef
getBufferWithName bufName = withEditor $ do
  bs <- gets $ findBufferWithName bufName
  case bs of
    [] -> fail ("Buffer not found: " ++ T.unpack bufName)
    b:_ -> return b

------------------------------------------------------------------------
-- | Perform action with any given buffer, using the last window that
-- was used for that buffer.
withGivenBuffer :: MonadEditor m => BufferRef -> BufferM a -> m a
withGivenBuffer k f = do
    b <- gets (findBufferWith k)

    withGivenBufferAndWindow (b ^. lastActiveWindowA) k f

-- | Perform action with any given buffer
withGivenBufferAndWindow :: MonadEditor m
    => Window -> BufferRef -> BufferM a -> m a
withGivenBufferAndWindow w k f = withEditor $ do
  accum <- asks configKillringAccumulate
  let edit e = let b = findBufferWith k e
                   (v, us, b') = runBufferFull w b f
               in (e & buffersA .~ mapAdjust' (const b') k (buffers e)
                     & killringA %~
                        if accum && all updateIsDelete us
                        then foldl (.) id $ reverse [ krPut dir s
                                                    | Delete _ dir s <- us ]
                        else id

                  , (us, v))
  (us, v) <- getsAndModify edit

  updHandler <- return . bufferUpdateHandler =<< ask
  unless (null us || null updHandler) $
    forM_ updHandler (\h -> withGivenBufferAndWindow w k (h us))
  return v

-- | Perform action with current window's buffer
withCurrentBuffer :: MonadEditor m => BufferM a -> m a
withCurrentBuffer f = withEditor $ do
  w <- use currentWindowA
  withGivenBufferAndWindow w (bufkey w) f

withEveryBuffer :: MonadEditor m => BufferM a -> m [a]
withEveryBuffer action =
    withEditor (gets bufferStack) >>= mapM (`withGivenBuffer` action) . NE.toList

currentWindowA :: Lens' Editor Window
currentWindowA = windowsA . PL.focus

-- | Return the current buffer
currentBuffer :: Editor -> BufferRef
currentBuffer = NE.head . bufferStack

-----------------------
-- Handling of status

-- | Prints a message with 'defaultStyle'.
printMsg :: MonadEditor m => T.Text -> m ()
printMsg s = printStatus ([s], defaultStyle)

-- | Prints a all given messages with 'defaultStyle'.
printMsgs :: MonadEditor m => [T.Text] -> m ()
printMsgs s = printStatus (s, defaultStyle)

printStatus :: MonadEditor m => Status -> m ()
printStatus = setTmpStatus 1

-- | Set the "background" status line
setStatus :: MonadEditor m => Status -> m ()
setStatus = setTmpStatus maxBound

-- | Clear the status line
clrStatus :: EditorM ()
clrStatus = setStatus ([""], defaultStyle)

statusLine :: Editor -> [T.Text]
statusLine = fst . statusLineInfo

statusLineInfo :: Editor -> Status
statusLineInfo = snd . head . statusLines

setTmpStatus :: MonadEditor m => Int -> Status -> m ()
setTmpStatus delay s = withEditor $ do
  statusLinesA %= DelayList.insert (delay, s)
  -- also show in the messages buffer, so we don't loose any message
  bs <- gets (filter ((== MemBuffer "messages") . view identA) . M.elems . buffers)

  b <- case bs of
         (b':_) -> return $ bkey b'
         [] -> stringToNewBuffer (MemBuffer "messages") mempty
  let m = listify $ R.fromText <$> fst s
  withGivenBuffer b $ botB >> insertN (m `R.snoc` '\n')

-- ---------------------------------------------------------------------
-- kill-register (vim-style) interface to killring.
--
-- Note that our vim keymap currently has its own registers
-- and doesn't use killring.

-- | Put string into yank register
setRegE :: R.YiString -> EditorM ()
setRegE s = killringA %= krSet s

-- | Return the contents of the yank register
getRegE :: EditorM R.YiString
getRegE = uses killringA krGet

-- ---------------------------------------------------------------------
-- | Dynamically-extensible state components.
--
-- These hooks are used by keymaps to store values that result from
-- Actions (i.e. that restult from IO), as opposed to the pure values
-- they generate themselves, and can be stored internally.
--
-- The `dynamic' field is a type-indexed map.
--

-- | Retrieve a value from the extensible state
getEditorDyn :: (MonadEditor m, YiVariable a, Default a, Functor m) => m a
getEditorDyn = fromMaybe def <$> getDyn (use dynamicA) (assign dynamicA)

-- | Insert a value into the extensible state, keyed by its type
putEditorDyn :: (MonadEditor m, YiVariable a, Functor m) => a -> m ()
putEditorDyn = putDyn (use dynamicA) (assign dynamicA)

-- | Like fnewE, create a new buffer filled with the String @s@,
-- Switch the current window to this buffer. Doesn't associate any
-- file with the buffer (unlike fnewE) and so is good for popup
-- internal buffers (like scratch)
newBufferE :: BufferId   -- ^ buffer name
           -> YiString -- ^ buffer contents
           -> EditorM BufferRef
newBufferE f s = do
    b <- stringToNewBuffer f s
    switchToBufferE b
    return b

-- | Like 'newBufferE' but defaults to empty contents.
newEmptyBufferE :: BufferId -> EditorM BufferRef
newEmptyBufferE f = newBufferE f Yi.Rope.empty

alternateBufferE :: Int -> EditorM ()
alternateBufferE n = do
    Window { bufAccessList = lst } <- use currentWindowA
    if null lst || (length lst - 1) < n
      then fail "no alternate buffer"
      else switchToBufferE $ lst!!n

-- | Create a new zero size window on a given buffer
newZeroSizeWindow :: Bool -> BufferRef -> WindowRef -> Window
newZeroSizeWindow mini bk ref = Window mini bk [] 0 0 emptyRegion ref 0 Nothing

-- | Create a new window onto the given buffer.
newWindowE :: Bool -> BufferRef -> EditorM Window
newWindowE mini bk = newZeroSizeWindow mini bk . WindowRef <$> newRef

-- | Attach the specified buffer to the current window
switchToBufferE :: BufferRef -> EditorM ()
switchToBufferE bk = windowsA . PL.focus %= \w ->
  w & bufkeyA .~ bk
    & bufAccessListA %~ forceFold1 . (bufkey w:) . filter (bk /=)

-- | Switch to the buffer specified as parameter. If the buffer name
-- is empty, switch to the next buffer.
switchToBufferWithNameE :: T.Text -> EditorM ()
switchToBufferWithNameE "" = alternateBufferE 0
switchToBufferWithNameE bufName = switchToBufferE =<< getBufferWithName bufName

-- | Close a buffer.
-- Note: close the current buffer if the empty string is given
closeBufferE :: T.Text -> EditorM ()
closeBufferE nm = deleteBuffer =<< getBufferWithNameOrCurrent nm

getBufferWithNameOrCurrent :: MonadEditor m => T.Text -> m BufferRef
getBufferWithNameOrCurrent t = withEditor $
    case T.null t of
        True -> gets currentBuffer
        False -> getBufferWithName t


------------------------------------------------------------------------

-- | Close current buffer and window, unless it's the last one.
closeBufferAndWindowE :: EditorM ()
closeBufferAndWindowE = do
  -- Fetch the current buffer *before* closing the window.
  -- The tryCloseE, since it uses tabsA, will have the
  -- current buffer "fixed" to the buffer of the window that is
  -- brought into focus. If the current buffer is accessed after the
  -- tryCloseE then the current buffer may not be the same as the
  -- buffer before tryCloseE. This would be bad.
  b <- gets currentBuffer
  tryCloseE
  deleteBuffer b

-- | Rotate focus to the next window
nextWinE :: EditorM ()
nextWinE = windowsA %= PL.next

-- | Rotate focus to the previous window
prevWinE :: EditorM ()
prevWinE = windowsA %= PL.previous

-- | Swaps the focused window with the first window. Useful for
-- layouts such as 'HPairOneStack', for which the first window is the
-- largest.
swapWinWithFirstE :: EditorM ()
swapWinWithFirstE = windowsA %= swapFocus (fromJust . PL.moveTo 0)

-- | Moves the focused window to the first window, and moves all other
-- windows down the stack.
pushWinToFirstE :: EditorM ()
pushWinToFirstE = windowsA %= pushToFirst
  where
      pushToFirst ws = case PL.delete ws of
          Nothing -> ws
          Just ws' -> PL.insertLeft (ws ^. PL.focus) (fromJust $ PL.moveTo 0 ws')

-- | Swap focused window with the next one
moveWinNextE :: EditorM ()
moveWinNextE = windowsA %= swapFocus PL.next

-- | Swap focused window with the previous one
moveWinPrevE :: EditorM ()
moveWinPrevE = windowsA %= swapFocus PL.previous

-- | A "fake" accessor that fixes the current buffer after a change of
-- the current window.
--
-- Enforces invariant that top of buffer stack is the buffer of the
-- current window.
fixCurrentBufferA_ :: Lens' Editor Editor
fixCurrentBufferA_ = lens id (\_old new -> let
  ws = windows new
  b = findBufferWith (bufkey $ PL._focus ws) new
  newBufferStack = nub (bkey b NE.<| bufferStack new)
  -- make sure we do not hold to old versions by seqing the length.
  in NE.length newBufferStack `seq` new & bufferStackA .~ newBufferStack)

withWindowE :: Window -> BufferM a -> EditorM a
withWindowE w = withGivenBufferAndWindow w (bufkey w)

findWindowWith :: WindowRef -> Editor -> Window
findWindowWith k e =
    head $ concatMap (\win -> [win | wkey win == k]) $ windows e

-- | Return the windows that are currently open on the buffer whose
-- key is given
windowsOnBufferE :: BufferRef -> EditorM [Window]
windowsOnBufferE k = do
  ts <- use tabsA
  let tabBufEq = concatMap (\win -> [win | bufkey win == k]) . (^. tabWindowsA)
  return $ concatMap tabBufEq ts

-- | bring the editor focus the window with the given key.
--
-- Fails if no window with the given key is found.
focusWindowE :: WindowRef -> EditorM ()
focusWindowE k = do
    -- Find the tab index and window index
    ts <- use tabsA
    let check (False, i) win = if wkey win == k
                                    then (True, i)
                                    else (False, i + 1)
        check r@(True, _) _win = r

        searchWindowSet (False, tabIndex, _) ws =
            case foldl check (False, 0) (ws ^. tabWindowsA) of
                (True, winIndex) -> (True, tabIndex, winIndex)
                (False, _)       -> (False, tabIndex + 1, 0)
        searchWindowSet r@(True, _, _) _ws = r

    case foldl searchWindowSet  (False, 0, 0) ts of
        (False, _, _) -> fail $ "No window with key " ++ show k ++ "found. (focusWindowE)"
        (True, tabIndex, winIndex) -> do
            assign tabsA (fromJust $ PL.moveTo tabIndex ts)
            windowsA %= fromJust . PL.moveTo winIndex

-- | Split the current window, opening a second window onto current buffer.
-- TODO: unfold newWindowE here?
splitE :: EditorM ()
splitE = do
  w <- gets currentBuffer >>= newWindowE False
  windowsA %= PL.insertRight w

-- | Cycle to the next layout manager, or the first one if the current
-- one is nonstandard.
layoutManagersNextE :: EditorM ()
layoutManagersNextE = withLMStackE PL.next

-- | Cycle to the previous layout manager, or the first one if the
-- current one is nonstandard.
layoutManagersPreviousE :: EditorM ()
layoutManagersPreviousE = withLMStackE PL.previous

-- | Helper function for 'layoutManagersNext' and 'layoutManagersPrevious'
withLMStackE :: (PL.PointedList AnyLayoutManager
                -> PL.PointedList AnyLayoutManager)
            -> EditorM ()
withLMStackE f = askCfg >>= \cfg ->
  currentTabA . tabLayoutManagerA %= go (layoutManagers cfg)
  where
   go [] lm = lm
   go lms lm =
     case findPL (layoutManagerSameType lm) lms of
       Nothing -> head lms
       Just lmsPL -> f lmsPL ^. PL.focus

-- | Next variant of the current layout manager, as given by 'nextVariant'
layoutManagerNextVariantE :: EditorM ()
layoutManagerNextVariantE = currentTabA . tabLayoutManagerA %= nextVariant

-- | Previous variant of the current layout manager, as given by
-- 'previousVariant'
layoutManagerPreviousVariantE :: EditorM ()
layoutManagerPreviousVariantE =
  currentTabA . tabLayoutManagerA %= previousVariant

-- | Sets the given divider position on the current tab
setDividerPosE :: DividerRef -> DividerPosition -> EditorM ()
setDividerPosE ref = assign (currentTabA . tabDividerPositionA ref)

-- | Creates a new tab containing a window that views the current buffer.
newTabE :: EditorM ()
newTabE = do
    bk <- gets currentBuffer
    win <- newWindowE False bk
    ref <- newRef
    tabsA %= PL.insertRight (makeTab1 ref win)

-- | Moves to the next tab in the round robin set of tabs
nextTabE :: EditorM ()
nextTabE = tabsA %= PL.next

-- | Moves to the previous tab in the round robin set of tabs
previousTabE :: EditorM ()
previousTabE = tabsA %= PL.previous

-- | Moves the focused tab to the given index, or to the end if the
-- index is not specified.
moveTabE :: Maybe Int -> EditorM ()
moveTabE Nothing  = do
    count <- uses tabsA PL.length
    tabsA %= fromJust . PL.moveTo (pred count)
moveTabE (Just n) = do
    newTabs <- uses tabsA (PL.moveTo n)
    when (isNothing newTabs) failure
    assign tabsA $ fromJust newTabs
  where failure = fail $ "moveTab " ++ show n ++ ": no such tab"

-- | Deletes the current tab. If there is only one tab open then error out.
--   When the last tab is focused, move focus to the left, otherwise
--   move focus to the right.
deleteTabE :: EditorM ()
deleteTabE = tabsA %= fromMaybe failure . deleteTab
  where failure = error "deleteTab: cannot delete sole tab"
        deleteTab tabs = if PL.atEnd tabs
                         then PL.deleteLeft tabs
                         else PL.deleteRight tabs

-- | Close the current window. If there is only one tab open and the tab
-- contains only one window then do nothing.
tryCloseE :: EditorM ()
tryCloseE = do
    ntabs <- uses tabsA PL.length
    nwins <- uses windowsA PL.length
    unless (ntabs == 1 && nwins == 1) $ if nwins == 1
      -- Could the Maybe response from deleteLeft be used instead of the
      -- def 'if'?
      then tabsA %= fromJust . PL.deleteLeft
      else windowsA %= fromJust . PL.deleteLeft

-- | Make the current window the only window on the screen
closeOtherE :: EditorM ()
closeOtherE = windowsA %= PL.deleteOthers

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: MonadEditor m => m ()
shiftOtherWindow = withEditor $ do
  len <- uses windowsA PL.length
  if len == 1
    then splitE
    else nextWinE

-- | Execute the argument in the context of an other window. Create
-- one if necessary. The current window is re-focused after the
-- argument has completed.
withOtherWindow :: MonadEditor m => m a -> m a
withOtherWindow f = do
  shiftOtherWindow
  x <- f
  withEditor prevWinE
  return x

acceptedInputs :: EditorM [T.Text]
acceptedInputs = do
  km <- defaultKm <$> askCfg
  keymap <- withCurrentBuffer $ gets (withMode0 modeKeymap)
  let l = I.accepted 3 . I.mkAutomaton . extractTopKeymap . keymap $ km
  return $ fmap T.unwords l

-- | Shows the current key bindings in a new window
acceptedInputsOtherWindow :: EditorM ()
acceptedInputsOtherWindow = do
  ai <- acceptedInputs
  b <- stringToNewBuffer (MemBuffer "keybindings") (fromText $ T.unlines ai)
  w <- newWindowE False b
  windowsA %= PL.insertRight w

addJumpHereE :: EditorM ()
addJumpHereE = addJumpAtE =<< withCurrentBuffer pointB

addJumpAtE :: Point -> EditorM ()
addJumpAtE point = do
  w <- use currentWindowA
  shouldAddJump <- case jumpList w of
    Just (PL.PointedList _ (Jump mark bf) _) -> do
      bfStillAlive <- gets (M.lookup bf . buffers)
      case bfStillAlive of
        Nothing -> return False
        _ -> do
          p <- withGivenBuffer bf . use $ markPointA mark
          return $! (p, bf) /= (point, bufkey w)
    _ -> return True
  when shouldAddJump $ do
    m <- withCurrentBuffer setMarkHereB
    let bf = bufkey w
        j = Jump m bf
    assign currentWindowA $ w & jumpListA %~ addJump j
    return ()

jumpBackE :: EditorM ()
jumpBackE = addJumpHereE >> modifyJumpListE jumpBack

jumpForwardE :: EditorM ()
jumpForwardE = modifyJumpListE jumpForward

modifyJumpListE :: (JumpList -> JumpList) -> EditorM ()
modifyJumpListE f = do
  w <- use currentWindowA
  case f $ w ^. jumpListA of
    Nothing -> return ()
    Just (PL.PointedList _ (Jump mark bf) _) -> do
      switchToBufferE bf
      withCurrentBuffer $ use (markPointA mark) >>= moveTo
      currentWindowA . jumpListA %= f

-- | Creates an in-memory buffer with a unique name.
newTempBufferE :: EditorM BufferRef
newTempBufferE = do
  e <- gets id
  -- increment the index of the hint until no buffer is found with that name
  let find_next currentName (nextName:otherNames) =
          case findBufferWithName currentName e of
            (_b : _) -> find_next nextName otherNames
            []      -> currentName
      find_next _ [] = error "Looks like nearly infinite list has just ended."
      next_tmp_name = find_next name names
      (name : names) = (fmap (("tmp-" Mon.<>) . T.pack . show) [0 :: Int ..])

  newEmptyBufferE (MemBuffer next_tmp_name)
