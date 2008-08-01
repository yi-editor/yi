{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
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

import Yi.Buffer
import Yi.Buffer.HighLevel

import Foundation
import AppKit

import qualified AppKit.NSScrollView (contentView)

$(declareClass "YiTextView" "NSTextView")
$(exportClass "YiTextView" "ytv_" [
    InstanceVariable "runBuffer" [t| BufferM () -> IO () |] [| \_ -> return () |]
  , InstanceMethod 'mouseDown -- '
  ])

ytv_mouseDown :: forall t. NSEvent t -> YiTextView () -> IO ()
ytv_mouseDown event self = do
  -- Determine the starting location before tracking mouse
  layout <- self # layoutManager
  container <- self # textContainer
  mousewin <- event # locationInWindow
  NSPoint ex ey <- self # convertPointFromView mousewin nil
  NSPoint ox oy <- self # textContainerOrigin
  let mouse@(NSPoint mx _) = NSPoint (ex - ox) (ey - oy)
  index <- layout # glyphIndexForPointInTextContainer mouse container >>= return . fromEnum
  NSRect (NSPoint cx _) (NSSize cw _) <-
    layout # boundingRectForGlyphRangeInTextContainer (NSRange (toEnum index) 1) container
  -- TODO: Is this ok? Is startIndex a utf8 index or a point?
  let startIndex = if mx - cx < cx + cw - mx then (Point index) else (Point index + 1)

  -- The super-class deals Cocoa-ishly with mouse events
  super self # mouseDown event

  -- Update our selection marker and position to reflect what Cocoa wants
  NSRange p l <- selectedRange self
  let p1 = fromIntegral p
      p2 = p1 + fromIntegral l
  runbuf <- self #. _runBuffer
  runbuf $ do
    setVisibleSelection (p1 /= p2)
    if p1 == p2
      then do
        moveTo p1 
      else if abs (startIndex - p2) < min (abs (startIndex - p1)) 2
        then setSelectionMarkPointB p2 >> moveTo p1
        else setSelectionMarkPointB p1 >> moveTo p2

$(declareClass "YiScrollView" "NSScrollView")
$(exportClass "YiScrollView" "ysv_" [
    InstanceVariable "leftScroller" [t| Bool |] [| False |]
  , InstanceMethod 'tile -- '
  ])
  
ysv_tile self = do
  super self # tile
  moveScroller <- self #. _leftScroller
  if not moveScroller
    then return ()
    else do
      -- Copied from NostalgicScrollView (found on /.)
      c <- self # AppKit.NSScrollView.contentView
      s <- self # verticalScroller
      sf <- s # frame
      s # setFrameOrigin (NSPoint 0.0 (nsMinY sf))
      c # frame >>= (flip setFrameOrigin c) . (NSPoint (nsWidth sf)) . nsMinY
