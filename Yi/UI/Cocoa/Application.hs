{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ForeignFunctionInterface #-}

--
-- Copyright (c) 2008 Gustav Munkby
--

-- | This module defines Cocoa event handling

-- For a Cocoa application to work we need to have the Cocoa
-- event-loop running. Since we don't want to re-implement the
-- event-loop in Haskell, we simply dispatch to the Objective-C
-- version and hook into events.
--
-- This however, is not completely trivial, since calling the
-- long-running Objective-C loop causes Haskell code not to be
-- executed at all. Upon receiving an event we must also make
-- sure to dispatch to other Haskell threads in order to make
-- progress.


module Yi.UI.Cocoa.Application
  ( YiApplication
  , _YiApplication
  , YiController
  , _YiController
  , initializeClass_Application
  , _eventChannel
  , setAppleMenu
  ) where

import Control.Concurrent
import Control.Monad

import Data.Bits

import Yi.Debug
import Yi.Event
import Yi.UI.Cocoa.Utils

import Foreign.C
 
import HOC ()

import Foundation hiding (name, new, parent, error, self, null)
import AppKit hiding (windows, start, rect, width, content, prompt, dictionary, icon, concat)

-- The selector is used since NSEvent#type treats the c enum
-- in a type-safe way, but Cocoa receives values which are not
-- defined in the c enum, which results in a pattern mismatch...
$(declareRenamedSelector "type" "rawType" [t| IO CInt |])
instance Has_rawType (NSEvent a)

-- This declares an application delegate which ensures the application
-- terminates when the last (and only) cocoa window is closed
$(declareClass "YiController" "NSObject")
$(exportClass "YiController" "yc_" [
    InstanceMethod 'applicationShouldTerminateAfterLastWindowClosed -- '
  ])

yc_applicationShouldTerminateAfterLastWindowClosed :: forall t. NSApplication t -> YiController () -> IO Bool
yc_applicationShouldTerminateAfterLastWindowClosed _app _self = return True

------------------------------------------------------------------------

-- This declares an application subclass which enables us to insert
-- ourselves into the application event loop and trap key-events application wide
$(declareClass "YiApplication" "NSApplication")
$(declareSelector "doTick" [t| IO () |])
$(declareSelector "setAppleMenu:" [t| forall t. NSMenu t -> IO () |] )
instance Has_setAppleMenu (NSApplication a)
$(exportClass "YiApplication" "ya_" [
    InstanceVariable "eventChannel" [t| Maybe (Yi.Event.Event -> IO ()) |] [| Nothing |]
  , InstanceMethod 'run -- '
  , InstanceMethod 'doTick -- '
  , InstanceMethod 'sendEvent -- '
  ])

ya_doTick :: YiApplication () -> IO ()
ya_doTick _ = replicateM_ 4 yield

ya_run :: YiApplication () -> IO ()
ya_run self = do
  -- Schedule a timer that repeatedly invokes ya_doTick in order to have
  -- some Haskell code running all the time. This will prevent other
  -- Haskell threads to stall while waiting for the Cocoa run loop to finish.
  _NSTimer # scheduledTimerWithTimeIntervalTargetSelectorUserInfoRepeats
                0.05 self (getSelectorForName "doTick") nil True
  super self # run

ya_sendEvent :: forall t. NSEvent t -> YiApplication () -> IO ()
ya_sendEvent event self = logNSException "sendEvent" $ do
  t <- event # rawType
  if t == fromCEnum nsKeyDown
    then self #. _eventChannel >>= handleKeyEvent event
    else super self # sendEvent event

handleKeyEvent :: forall t. NSEvent t -> Maybe (Yi.Event.Event -> IO ()) -> IO ()
handleKeyEvent event ch = do
  mask <- event # modifierFlags
  str <- event # charactersIgnoringModifiers >>= haskellString
  logPutStrLn $ "Key " ++ str
  let (k,shift') = case str of
                "\r"     -> (Just KEnter, True)
                "\t"     -> (Just KTab, True)
                "\DEL"   -> (Just KBS, True)
                "\ESC"   -> (Just KEsc, True)
                "\63232" -> (Just KUp, True)
                "\63233" -> (Just KDown, True)
                "\63234" -> (Just KLeft, True)
                "\63235" -> (Just KRight, True)
                "\63273" -> (Just KHome, True)
                "\63275" -> (Just KEnd, True)
                "\63276" -> (Just KPageUp, True)
                "\63277" -> (Just KPageDown, True)
                [c]      -> (Just $ KASCII c, False)
                _        -> (Nothing, True)
  case (k,ch) of
    (Just k, Just ch) -> ch (Event k (modifiers shift' mask))
    _                 -> return ()

modifierTable :: Bool -> [(CUInt, Modifier)]
modifierTable False = [(bit 18,MCtrl), (bit 19,MMeta)]
modifierTable True  = (bit 17,MShift) : modifierTable False

modifiers :: Bool -> CUInt -> [Modifier]
modifiers shift' mask = [yi | (cocoa, yi) <- modifierTable shift', (cocoa .&. mask) /= 0]

initializeClass_Application :: IO ()
initializeClass_Application = do
  initializeClass_YiApplication
  initializeClass_YiController
