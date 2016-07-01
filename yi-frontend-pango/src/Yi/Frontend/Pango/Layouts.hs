{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.UI.Pango.Layouts
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides abstract controls which implement 'Yi.Layout.Layout's and
-- which manage the minibuffer.
--
-- The implementation strategy is to first construct the layout
-- managers @WeightedStack@ (implementing the 'Stack' constructor) and
-- @SlidingPair@ (implementing the 'Pair' constructor), and then
-- construct 'LayoutDisplay' as a tree of these, mirroring the
-- structure of 'Layout'.

module Yi.UI.Pango.Layouts (
  -- * Getting the underlying widget
  WidgetLike(..),
  -- * Window layout
  LayoutDisplay,
  layoutDisplayNew,
  layoutDisplaySet,
  layoutDisplayOnDividerMove,
  -- * Miniwindow layout
  MiniwindowDisplay,
  miniwindowDisplayNew,
  miniwindowDisplaySet,
  -- * Tabs
  SimpleNotebook,
  simpleNotebookNew,
  simpleNotebookSet,
  simpleNotebookOnSwitchPage,
  -- * Utils
  update,
 ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad hiding (mapM, forM)
import           Data.Foldable (toList)
import           Data.IORef
import qualified Data.List.PointedList as PL
import qualified Data.Text as T
import           Data.Traversable
import           Graphics.UI.Gtk as Gtk hiding(Orientation, Layout)
import           Prelude hiding (mapM)
import           Yi.Layout(Orientation(..), RelativeSize, DividerPosition,
                           Layout(..), DividerRef)

class WidgetLike w where
  -- | Extracts the main widget. This is the widget to be added to the GUI.
  baseWidget :: w -> Widget

----------------------- The WeightedStack type
{- | A @WeightedStack@ is like a 'VBox' or 'HBox', except that we may
specify the ratios of the areas of the child widgets (so this
implements the 'Stack' constructor of 'Yi.Layout.Layout'.

Essentially, we implement this layout manager from scratch, by
implementing the 'sizeRequest' and 'sizeAllocate' signals by hand (see
the 'Container' documentation for details, and
http://www.ibm.com/developerworks/linux/library/l-widget-pygtk/ for an
example in Python). Ideally, we would directly subclass the abstract
class 'Container', but Gtk2hs doesn't directly support this. Instead,
we start off with the concrete class 'Fixed', and just override its
layout behaviour.
-}

newtype WeightedStack = WS Fixed
  deriving(GObjectClass, ObjectClass, WidgetClass,ContainerClass)

type StackDescr = [(Widget, RelativeSize)]

weightedStackNew :: Orientation -> StackDescr -> IO WeightedStack
weightedStackNew o s = do
  when (any ((<= 0) . snd) s) $ error
    "Yi.UI.Pango.WeightedStack.WeightedStack: all weights must be positive"
  l <- fixedNew
  set l (fmap ((containerChild :=) . fst) s)
  void $ Gtk.on l sizeRequest (doSizeRequest o s)
  void $ Gtk.on l sizeAllocate (relayout o s)
  return (WS l)

-- | Requests the smallest size so that each widget gets its requested size
doSizeRequest :: Orientation -> StackDescr -> IO Requisition
doSizeRequest o s =
  let
    (requestAlong, requestAcross) =
      case o of
        Horizontal ->
          (\(Requisition w _) -> fromIntegral w,
           \(Requisition _ h) -> h)
        Vertical ->
          (\(Requisition _ h) -> fromIntegral h,
           \(Requisition w _) -> w)

    totalWeight = sum . fmap snd $ s
    reqsize (request, relSize) = requestAlong request / relSize
    sizeAlong widgetRequests =
      totalWeight * (maximum . fmap reqsize $ widgetRequests)
    sizeAcross widgetRequests =
      maximum . fmap (requestAcross . fst) $ widgetRequests
    mkRequisition wr =
      case o of
        Horizontal -> Requisition (round $ sizeAlong wr) (sizeAcross wr)
        Vertical -> Requisition (sizeAcross wr) (round $ sizeAlong wr)
    swreq (w, relSize) = (,relSize) <$> widgetSizeRequest w
  in
   boundRequisition =<< mkRequisition <$> mapM swreq s


-- | Bounds the given requisition to not exceed screen dimensions
boundRequisition :: Requisition -> IO Requisition
boundRequisition r@(Requisition w h) =
  do
    mscr <- screenGetDefault
    case mscr of
      Just scr -> Requisition <$> (min w <$> screenGetWidth scr)
                              <*> (min h <$> screenGetHeight scr)
      Nothing -> return r

-- | Position the children appropriately for the given width and height
relayout :: Orientation -> StackDescr -> Rectangle -> IO ()
relayout o s (Rectangle x y width height) =
  let
    totalWeight = sum . fmap snd $ s
    totalSpace = fromIntegral $
      case o of
        Horizontal -> width
        Vertical -> height
    wtMult = totalSpace / totalWeight
    calcPosition pos (widget, wt) = (pos + wt * wtMult,
                                     (pos, wt * wtMult, widget))
    widgetToRectangle (round -> pos, round -> size, widget) =
      case o of
        Horizontal -> (Rectangle pos y size height, widget)
        Vertical -> (Rectangle x pos width size, widget)
    startPosition = fromIntegral $
      case o of
        Horizontal -> x
        Vertical -> y
    widgetPositions =
      fmap widgetToRectangle (snd (mapAccumL calcPosition startPosition s))
  in forM_ widgetPositions $ \(rect, widget) -> widgetSizeAllocate widget rect

------------------------------------------------------- SlidingPair

{-|
'SlidingPair' implements the 'Pair' constructor.

Most of what is needed is already implemented by the 'HPaned' and
'VPaned' classes. The main feature added by 'SlidingPair' is that the
divider position, *as a fraction of the available space*, remains
constant even when resizing.
-}

newtype SlidingPair = SP Paned
  deriving(GObjectClass, ObjectClass, WidgetClass, ContainerClass)

slidingPairNew :: (WidgetClass w1, WidgetClass w2) => Orientation -> w1 -> w2
               -> DividerPosition
               -> (DividerPosition -> IO ())
               -> IO SlidingPair
slidingPairNew o w1 w2 pos handleNewPos = do
  p <-
    case o of
      Horizontal -> toPaned <$> hPanedNew
      Vertical -> toPaned <$> vPanedNew
  panedPack1 p w1 True True
  panedPack2 p w2 True True

{- We want to catch the sizeAllocate signal. If this event is
called, two things could have happened: the size could have changed;
or the slider could have moved.  We want to correct the slider
position, but only if the size has changed. Furthermore, if the size
only changes in the direction /orthogonal/ to the slider, then there
is also no need to correct the slider position.

-}

  posRef <- newIORef pos
  sizeRef <- newIORef 0

  void $ Gtk.on p sizeAllocate $ \(Rectangle _ _ w h) ->
    do
      oldSz <- readIORef sizeRef
      oldPos <- readIORef posRef

      let sz = case o of
            Horizontal -> w
            Vertical -> h
      writeIORef sizeRef sz
      when (sz /= 0) $
        if sz == oldSz
        then do -- the slider was moved; store its new position
          sliderPos <- get p panedPosition
          let newPos = fromIntegral sliderPos / fromIntegral sz
          writeIORef posRef newPos
          when (oldPos /= newPos) $ handleNewPos newPos
        else -- the size was changed; restore the slider position and
             -- save the new position
          set p [ panedPosition := round (oldPos * fromIntegral sz) ]

  return (SP p)

----------------------------- LayoutDisplay
-- | A container implements 'Layout's.
data LayoutDisplay
  = LD {
     mainWidget :: Bin,
     implWidget :: IORef (Maybe LayoutImpl),
     dividerCallbacks :: IORef [DividerRef -> DividerPosition -> IO ()]
     }

-- | Tree mirroring 'Layout', which holds the layout widgets for 'LayoutDisplay'
data LayoutImpl
  = SingleWindowI {
      singleWidget :: Widget
    }
  | StackI {
      orientationI :: Orientation,
      winsI :: [(LayoutImpl, RelativeSize)],
      stackWidget :: WeightedStack
    }
  | PairI {
      orientationI :: Orientation,
      pairFstI :: LayoutImpl,
      pairSndI :: LayoutImpl,
      divRefI :: DividerRef,
      pairWidget :: SlidingPair
    }

--- construction
layoutDisplayNew :: IO LayoutDisplay
layoutDisplayNew = do
  cbRef <- newIORef []
  implRef <- newIORef Nothing
  box <- toBin <$> alignmentNew 0 0 1 1
  return (LD box implRef cbRef)

-- | Registers a callback to a divider changing position. (There is
-- currently no way to unregister.)
layoutDisplayOnDividerMove :: LayoutDisplay
                           -> (DividerRef -> DividerPosition -> IO ())
                           -> IO ()
layoutDisplayOnDividerMove ld cb = modifyIORef (dividerCallbacks ld) (cb:)

--- changing the layout

-- | Sets the layout to the given schema.
--
-- * it is permissible to add or remove widgets in this process.
--
-- * as an optimisation, this function will first check whether the
-- layout has actually changed (so the caller need not be concerned
-- with this)
--
-- * will run 'widgetShowAll', and hence will show the underlying widgets too
layoutDisplaySet :: LayoutDisplay -> Layout Widget -> IO ()
layoutDisplaySet ld lyt = do
  mimpl <- readIORef (implWidget ld)

  let applyLayout = do
        impl' <- buildImpl (runCb $ dividerCallbacks ld) lyt
        widgetShowAll (outerWidget impl')
        set (mainWidget ld) [containerChild := outerWidget impl']
        writeIORef (implWidget ld) (Just impl')

  case mimpl of
    Nothing -> applyLayout
    Just impl -> unless (sameLayout impl lyt) $ do
      unattachWidgets (toContainer $ mainWidget ld) impl
      applyLayout

runCb :: IORef [DividerRef -> DividerPosition -> IO ()]
      -> DividerRef -> DividerPosition -> IO ()
runCb cbRef dRef dPos = readIORef cbRef >>= mapM_ (\cb -> cb dRef dPos)

buildImpl :: (DividerRef -> DividerPosition -> IO ())
          -> Layout Widget -> IO LayoutImpl
buildImpl cb = go
  where
    go (SingleWindow w) = return (SingleWindowI w)
    go (s@Stack{}) = do
      impls <- forM (wins s) $ \(lyt,relSize) -> (,relSize) <$> go lyt
      ws <- weightedStackNew (orientation s) (first outerWidget <$> impls)
      return (StackI (orientation s) impls ws)
    go (p@Pair{}) = do
      w1 <- go (pairFst p)
      w2 <- go (pairSnd p)
      sp <- slidingPairNew (orientation p) (outerWidget w1)
                           (outerWidget w2) (divPos p) (cb $ divRef p)
      return $ PairI (orientation p) w1 w2 (divRef p) sp

-- | true if the displayed layout agrees with the given schema, other
-- than divider positions
sameLayout :: LayoutImpl -> Layout Widget -> Bool
sameLayout (SingleWindowI w) (SingleWindow w') = w == w'
sameLayout (s@StackI{}) (s'@Stack{}) =
     orientationI s == orientation s'
  && length (winsI s) == length (wins s')
  && and (zipWith (\(impl, relSize) (layout, relSize') ->
                    relSize == relSize' && sameLayout impl layout)
          (winsI s) (wins s'))
sameLayout (p@PairI{}) (p'@Pair{}) =
     orientationI p == orientation p'
  && divRefI p == divRef p'
  && sameLayout (pairFstI p) (pairFst p')
  && sameLayout (pairSndI p) (pairSnd p')
sameLayout _ _ = False

-- removes all widgets from the layout
unattachWidgets :: Container -> LayoutImpl -> IO ()
unattachWidgets parent (SingleWindowI w) = containerRemove parent w
unattachWidgets parent s@StackI{} = do
  containerRemove parent (stackWidget s)
  mapM_ (unattachWidgets (toContainer $ stackWidget s) . fst) (winsI s)
unattachWidgets parent p@PairI{} = do
  containerRemove parent (pairWidget p)
  mapM_ (unattachWidgets (toContainer $ pairWidget p)) [pairFstI p, pairSndI p]


-- extract the main widget from the tree
outerWidget :: LayoutImpl -> Widget
outerWidget s@SingleWindowI{} = singleWidget s
outerWidget s@StackI{} = toWidget . stackWidget $ s
outerWidget p@PairI{} = toWidget . pairWidget $ p

instance WidgetLike LayoutDisplay where
  baseWidget = toWidget . mainWidget

---------------- MiniwindowDisplay
data MiniwindowDisplay
  = MD
   { mwdMainWidget :: VBox,
     mwdWidgets :: IORef [Widget]
   }

miniwindowDisplayNew :: IO MiniwindowDisplay
miniwindowDisplayNew = do
  vb <- vBoxNew False 1
  wsRef <- newIORef []
  return (MD vb wsRef)

instance WidgetLike MiniwindowDisplay where
  baseWidget = toWidget . mwdMainWidget

miniwindowDisplaySet :: MiniwindowDisplay -> [Widget] -> IO ()
miniwindowDisplaySet mwd ws = do
  curWs <- readIORef (mwdWidgets mwd)

  -- we could be more careful here, and only remove the widgets which we need to.
  when (ws /= curWs) $ do
    forM_ curWs $ containerRemove (mwdMainWidget mwd)
    forM_ ws $ \w -> boxPackEnd (mwdMainWidget mwd) w PackNatural 0
    widgetShowAll $ mwdMainWidget mwd
    writeIORef (mwdWidgets mwd) ws


---------------------- SimpleNotebook
data SimpleNotebook
   = SN
    { snMainWidget :: Notebook,
      snTabs :: IORef (Maybe (PL.PointedList (Widget, T.Text)))
    }

instance WidgetLike SimpleNotebook where
  baseWidget = toWidget . snMainWidget

-- | Constructs an empty notebook
simpleNotebookNew :: IO SimpleNotebook
simpleNotebookNew = do
  nb <- notebookNew
  ts <- newIORef Nothing
  return (SN nb ts)

-- | Sets the tabs
simpleNotebookSet :: SimpleNotebook -> PL.PointedList (Widget, T.Text) -> IO ()
simpleNotebookSet sn ts = do
  curTs <- readIORef (snTabs sn)

  let nb = snMainWidget sn
      tsList = toList ts
      curTsList = maybe [] toList curTs

  -- the common case is no change at all
  when (curTs /= Just ts) $ do

    -- update the tabs, if they have changed
    when (fmap fst curTsList /= fmap fst tsList) $ do
      forM_ curTsList $ const (notebookRemovePage nb (-1))
      forM_ tsList $ uncurry (notebookAppendPage nb)

    -- now update the titles if they have changed
    forM_ tsList $ \(w,s) -> update nb (notebookChildTabLabel w) s

    -- now set the focus
    p <- notebookPageNum nb (fst $ PL._focus ts)
    maybe (return ()) (update nb notebookPage) p

    -- write the new status
    writeIORef (snTabs sn) (Just ts)

    -- display!
    widgetShowAll nb


-- | The 'onSwitchPage' callback
simpleNotebookOnSwitchPage :: SimpleNotebook -> (Int -> IO ()) -> IO ()
simpleNotebookOnSwitchPage sn = void . (snMainWidget sn `on` switchPage)


------------------- Utils
-- Only set an attribute if has actually changed.
-- This makes setting window titles much faster.
update :: (Eq a) => o -> ReadWriteAttr o a a -> a -> IO ()
update w attr val = do oldVal <- get w attr
                       when (val /= oldVal) $ set w [attr := val]
