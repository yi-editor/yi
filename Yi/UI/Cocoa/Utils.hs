{-# LANGUAGE Rank2Types #-}
--
-- Copyright (c) 2008 Gustav Munkby
--

-- | Helper functions for the Cocoa frontend

module Yi.UI.Cocoa.Utils where

import Prelude hiding (init)
import Yi.Debug
import Yi.Region
import Yi.Style

import Control.Applicative
import Control.Concurrent

-- Specify Cocoa imports explicitly, to avoid name-clashes.
-- Since the number of functions recognized by HOC varies
-- between revisions, this seems like the safest choice.
import HOC
import Foundation (
  NSDictionary,NSMutableDictionary,NSObject,NSObject_,
  _NSMutableDictionary,alloc,autorelease,catchNS,description,
  haskellString,retain,setValueForKey,dictionary,init,
  objectEnumerator,nextObject,NSArray,NSRange(..))
import AppKit (
  Has_setFont,NSColor,_NSColor,_NSFont,blackColor,
  colorWithDeviceRedGreenBlueAlpha,nsBackgroundColorAttributeName,
  nsFontAttributeName,nsForegroundColorAttributeName,setFont,
  userFixedPitchFontOfSize,whiteColor)

logNSException :: String -> IO () -> IO ()
logNSException str act =
  catchNS act (\e -> description e >>= haskellString >>=
                     logPutStrLn . (("NSException " ++ str ++ ":") ++))


new, autonew :: forall t. Class (NSObject_ t) -> IO (NSObject t)
new x = do
  d <- description x >>= haskellString
  o <- alloc x
  logPutStrLn $ "New " ++ d
  init o
autonew x = new x >>= autoreleased

autoreleased :: forall t. NSObject t -> IO (NSObject t)
autoreleased o = do
  retain o
  autorelease o
  return o

setMonospaceFont :: Has_setFont v => v -> IO ()
setMonospaceFont view = do
  userFixedPitchFontOfSize 0 _NSFont >>= flip setFont view

-- | Convert style information into Cocoa compatible format
convertAttributes :: Attributes -> IO (NSDictionary ())
convertAttributes s = do
  d <- castObject <$> dictionary _NSMutableDictionary
  ft <- userFixedPitchFontOfSize 0 _NSFont
  setValueForKey ft nsFontAttributeName d
  fillAttributeDict d s
  castObject <$> return d

-- | Fill and return the filled dictionary with the style information
fillAttributeDict :: NSMutableDictionary t -> Attributes -> IO ()
fillAttributeDict d a = do
  getColor True  (foreground a) >>= setForKey nsForegroundColorAttributeName
  getColor False (background a) >>= setForKey nsBackgroundColorAttributeName
  where setForKey k = \v -> setValueForKey v k d

-- | Convert a Yi color into a Cocoa color
getColor :: Bool -> Color -> IO (NSColor ())
getColor fg Default = if fg then _NSColor # blackColor else _NSColor # whiteColor
getColor _g (RGB r g b) =
  let conv = (/255) . fromIntegral in
  _NSColor # colorWithDeviceRedGreenBlueAlpha (conv r) (conv g) (conv b) 1.0

haskellList :: forall t1. NSArray t1 -> IO [ID ()]
haskellList a = a # objectEnumerator >>= helper
  where
    helper enum = do
      e <- enum # nextObject
      if e == nil
        then return []
        else helper enum >>= return . (e :)

mkRangeRegion :: NSRange -> Region
mkRangeRegion (NSRange i l) = mkSizeRegion (fromIntegral i) (fromIntegral l)

mkRegionRange :: Region -> NSRange
mkRegionRange r = NSRange (fromIntegral $ regionStart r) (fromIntegral $ regionSize r)

-- NOTE: Should really be in HOC... we can't even give a type signature here...
modifyIVar ivar f slf = modifyMVar (getInstanceMVar ivar slf) f
modifyIVar_ ivar f slf = modifyMVar_ (getInstanceMVar ivar slf) f

{-

-- Debugging helpers

data Hierarchy = View String NSRect [Hierarchy]
  deriving Show


mkHierarchy :: forall t. NSView t -> IO Hierarchy
mkHierarchy v = do
  d <- v # description >>= haskellString
  f <- v # frame
  ss <- v # subviews >>= haskellList >>= mapM (mkHierarchy . toNSView)
  return $ View d f ss

-}
