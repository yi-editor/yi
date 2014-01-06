{-# LANGUAGE TemplateHaskell, EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
--
-- Copyright (c) 2008 Gustav Munkby
--

-- | This declares our special text-view class. The textview interpretes
--   mouse events so that mouse-selection in Yi should function as in any
--   Cocoa application

module Yi.UI.Cocoa.TextView
  ( YiTextView
  , _YiTextView
  , initializeClass_YiTextView
  , _runBuffer
  , YiScrollView
  , _YiScrollView
  , initializeClass_YiScrollView
  , _leftScroller
  , visibleRange
  )where



import Yi.String
import Yi.Buffer hiding (runBuffer)
import Yi.UI.Cocoa.Utils

-- Specify Cocoa imports explicitly, to avoid name-clashes.
-- Since the number of functions recognized by HOC varies
-- between revisions, this seems like the safest choice.
import HOC
import Foundation (
  NSPoint(..),NSRange(..),nsMinY,nsWidth,nsOffsetRect,NSArray,
  addObject,haskellString,NSMutableArray,NSValue,nsUnionRange,
  _NSMutableArray,array,addObjectsFromArray,rangeValue)
import AppKit (
  NSSelectionAffinity,characterRangeForGlyphRangeActualGlyphRange,
  glyphRangeForBoundingRectInTextContainer,layoutManager,textContainer,
  textContainerOrigin,visibleRect,frame,verticalScroller,NSTextView,
  NSTextViewClass,setSelectedRangesAffinityStillSelecting,NSScrollView,
  NSScrollViewClass,tile,setFrameOrigin,performDragOperation,
  NSTextViewMetaClass,NSScrollViewMetaClass,
  acceptableDragTypes,nsStringPboardType,stringForType,NSPasteboard,
  convertPointFromView,availableTypeFromArray,_NSPasteboard,
  typesFilterableTo,nsFilenamesPboardType,propertyListForType,
  glyphIndexForPointInTextContainerFractionOfDistanceThroughGlyph)

import qualified AppKit.NSScrollView (contentView)

import Foreign
import Foreign.C

-- TODO: The correct way of doing this would be to add the
--       protocol constraints on the performDragOperation
--       parameter, but for whatever reason, HOC doesn't
--       do this, so we use this hack to work around it...
$(declareRenamedSelector "draggingLocation" "draggingLocation" [t| IO NSPoint |])
$(declareRenamedSelector "draggingSource" "draggingSource" [t| IO (ID ()) |])
$(declareRenamedSelector "draggingPasteboard" "draggingPasteboard" [t| IO (NSPasteboard ()) |])
instance Has_draggingPasteboard (ID t)
instance Has_draggingSource (ID t)
instance Has_draggingLocation (ID t)
_silenceWarning :: (
  ImpType_draggingPasteboard a b,
  ImpType_draggingSource c d,
  ImpType_draggingLocation e f)
_silenceWarning = undefined


$(declareClass "YiTextView" "NSTextView")
$(exportClass "YiTextView" "ytv_" [
    InstanceVariable "runBuffer" [t| BufferM () -> IO () |] [| const $ return () |]
  , InstanceVariable "selectingPosition" [t| Maybe Int |] [| Nothing |]
  , InstanceMethod 'setSelectedRangesAffinityStillSelecting -- '
  , InstanceMethod 'acceptableDragTypes
  , InstanceMethod 'performDragOperation
  ])

-- | Intercept mouse selection so that we can update Yi's selection
--   according to how Cocoa wants it.
ytv_setSelectedRangesAffinityStillSelecting :: NSArray () -> NSSelectionAffinity -> Bool -> YiTextView () -> IO ()
ytv_setSelectedRangesAffinityStillSelecting rs a b v = do
  hrs <- fmap castObject <$> haskellList rs :: IO [NSValue ()]
  r@(NSRange i len) <- foldlM nsUnionRange (NSRange 0 0) =<< mapM rangeValue hrs
  p <- v #. _selectingPosition
  case (b, p) of
    (True, Nothing) -> do
      -- Assume that the initial indication gives starting position
      v # setIVar _selectingPosition (Just $ fromIntegral i)
    (False, Just p0) -> do
      v # setIVar _selectingPosition Nothing
      runbuf <- v #. _runBuffer
      runbuf $ do
        setVisibleSelection (len /= 0)
        setSelectionMarkPointB (fromIntegral p0)
        moveTo (fromIntegral $ if fromIntegral i == p0 then i + len else i)
    _ -> do
      -- Ignore intermediate updates (Cocoa buffers events until selection finishes)
      -- Ignore direct updates (to avoid having to detect "our" updates)
      return ()

  super v # setSelectedRangesAffinityStillSelecting rs a b

ytv_acceptableDragTypes :: YiTextView () -> IO (NSArray ())
ytv_acceptableDragTypes _ = do
  ar <- castObject <$> _NSMutableArray # array :: IO (NSMutableArray ())
  _NSPasteboard # typesFilterableTo nsStringPboardType >>=
    flip addObjectsFromArray ar
  ar # addObject nsFilenamesPboardType
  return (castObject ar)

-- Implement support for drag and drop...
ytv_performDragOperation :: ID t -> YiTextView () -> IO Bool
ytv_performDragOperation dragInfo slf = do
  pb <- dragInfo # draggingPasteboard

  ty <- slf # ytv_acceptableDragTypes >>= flip availableTypeFromArray pb

  when (ty /= nil) $ do
    str <-
      if ty == nsFilenamesPboardType
        then unlines' <$>
          (pb # propertyListForType ty >>= (haskellList.castObject) >>= mapM (haskellString.castObject))
        else pb # stringForType ty >>= haskellString

    src <- dragInfo # draggingSource
    -- Apparently, the selection only looks as if it was updated...
    -- we would have to use draggingLocation to figure out where
    -- the new text should go. Which means that we need to get back
    -- our old trick for translating a mouse-position to a text-position.
    pIns <- dragInfo # draggingLocation >>= flip charAtMouse slf

    runbuf <- slf #. _runBuffer
    runbuf $ do
      hasSel <- use highlightSelectionA
      rSel <- if hasSel && src == (castObject slf) then getSelectRegionB else return emptyRegion

      moveTo $ fromIntegral pIns
      -- To not affect positions we make the "latter" modification first
      -- Note that there will be no drag operation if they overlap...
      if regionStart rSel < fromIntegral pIns
        then insertN str >> deleteRegionB rSel
        else deleteRegionB rSel >> insertN str

  return (ty /= nil)

-- | Compute the character index of the specified mouse position
charAtMouse :: NSPoint -> YiTextView () -> IO CUInt
charAtMouse p slf = do
  -- Determine the text-relative coordinate
  container <- slf # textContainer
  NSPoint ex ey <- slf # convertPointFromView p nil
  NSPoint ox oy <- slf # textContainerOrigin
  let mouse = NSPoint (ex - ox) (ey - oy)
  -- Determine the index
  layout <- slf # layoutManager
  pf <- malloc -- I miss stack variables from C... =P
  index <- layout # glyphIndexForPointInTextContainerFractionOfDistanceThroughGlyph mouse container pf
  fract <- peek pf
  free pf
  return $ if (fract > 0.5) then index + 1 else index

-- | Compute the currently visible text range in the view
visibleRange :: YiTextView () -> IO NSRange
visibleRange v = do
  -- Force redraw of the whole container to capture interactive style changes...
  NSPoint x y <- v # textContainerOrigin
  r <- v # visibleRect >>= \r -> nsOffsetRect r x y
  lm <- v # layoutManager
  tc <- v # textContainer
  gr <- lm # glyphRangeForBoundingRectInTextContainer r tc
  lm # characterRangeForGlyphRangeActualGlyphRange gr nullPtr

$(declareClass "YiScrollView" "NSScrollView")
$(exportClass "YiScrollView" "ysv_" [
    InstanceVariable "leftScroller" [t| Bool |] [| False |]
  , InstanceMethod 'tile -- '
  ])

ysv_tile :: YiScrollView () -> IO ()
ysv_tile slf = do
  super slf # tile
  moveScroller <- slf #. _leftScroller
  if not moveScroller
    then return ()
    else do
      -- Copied from NostalgicScrollView (found on /.)
      c <- slf # AppKit.NSScrollView.contentView
      s <- slf # verticalScroller
      sf <- s # frame
      s # setFrameOrigin (NSPoint 0.0 (nsMinY sf))
      c # frame >>= (flip setFrameOrigin c) . (NSPoint (nsWidth sf)) . nsMinY
