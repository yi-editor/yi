{-# LANGUAGE TemplateHaskell, EmptyDataDecls, MultiParamTypeClasses #-}
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
  )where

import Yi.Buffer hiding (runBuffer)
import Yi.Buffer.HighLevel

import Foundation
import AppKit

import qualified AppKit.NSScrollView (contentView)

import Foreign.C

$(declareClass "YiTextView" "NSTextView")
$(exportClass "YiTextView" "ytv_" [
    InstanceVariable "runBuffer" [t| BufferM () -> IO () |] [| \_ -> return () |]
  , InstanceVariable "selectingPosition" [t| Maybe CUInt |] [| Nothing |]
  , InstanceMethod 'setSelectedRangeAffinityStillSelecting -- '
  ])

-- | Intercept mouse selection so that we can update Yi's selection
--   according to how Cocoa wants it.
ytv_setSelectedRangeAffinityStillSelecting :: NSRange -> NSSelectionAffinity -> Bool -> YiTextView () -> IO ()
ytv_setSelectedRangeAffinityStillSelecting r@(NSRange p1 l) a b v = do
  p <- v #. _selectingPosition
  case (b, p) of
    (True, Nothing) -> do
      -- Assume that the initial indication gives starting position
      v # setIVar _selectingPosition (Just p1)
    (False, Just p0) -> do
      v # setIVar _selectingPosition Nothing
      runbuf <- v #. _runBuffer
      runbuf $ do
        setVisibleSelection (l /= 0)
        setSelectionMarkPointB (fromIntegral p0)
        moveTo (fromIntegral $ if p1 == p0 then p1 + l else p1)
    _ -> do
      -- Ignore intermediate updates (Cocoa buffers events until selection finishes)
      -- Ignore direct updates (to avoid having to detect "our" updates)
      return ()

  super v # setSelectedRangeAffinityStillSelecting r a b

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
