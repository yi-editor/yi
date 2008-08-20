--
-- Copyright (c) 2008 Gustav Munkby
--

-- | Helper functions for the Cocoa frontend

module Yi.UI.Cocoa.Utils where

import Prelude hiding (init)
import Yi.Debug
import Yi.Style

import Control.Applicative

import Foundation hiding (new)
import AppKit hiding (dictionary)

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
convertStyle :: Style -> IO (NSDictionary ())
convertStyle s = do
  d <- castObject <$> dictionary _NSMutableDictionary
  ft <- userFixedPitchFontOfSize 0 _NSFont
  setValueForKey ft nsFontAttributeName d
  fillStyleDict d s
  castObject <$> return d

-- | Fill and return the filled dictionary with the style information
fillStyleDict :: NSMutableDictionary t -> Style -> IO ()
fillStyleDict _ [] = return ()
fillStyleDict d (x:xs) = do
  fillStyleDict d xs
  getDictStyle x >>= flip (uncurry setValueForKey) d

-- | Return a (value, key) pair for insertion into the style dictionary
getDictStyle :: Attr -> IO (NSColor (), NSString ())
getDictStyle (Foreground c) = (,) <$> getColor True c  <*> pure nsForegroundColorAttributeName
getDictStyle (Background c) = (,) <$> getColor False c <*> pure nsBackgroundColorAttributeName

-- | Convert a Yi color into a Cocoa color
getColor :: Bool -> Color -> IO (NSColor ())
getColor fg Default = if fg then _NSColor # blackColor else _NSColor # whiteColor
getColor fg Reverse = if fg then _NSColor # whiteColor else _NSColor # blackColor
getColor _g (RGB r g b) =
  let conv = (/255) . fromIntegral in
  _NSColor # colorWithDeviceRedGreenBlueAlpha (conv r) (conv g) (conv b) 1.0

-- Debugging helpers

{-

data Hierarchy = View String NSRect [Hierarchy]
  deriving Show

haskellList :: forall t1. NSArray t1 -> IO [ID ()]
haskellList a = a # objectEnumerator >>= helper
  where
    helper enum = do
      e <- enum # nextObject
      if e == nil
        then return []
        else helper enum >>= return . (e :)

mkHierarchy :: forall t. NSView t -> IO Hierarchy
mkHierarchy v = do
  d <- v # description >>= haskellString
  f <- v # frame
  ss <- v # subviews >>= haskellList >>= mapM (mkHierarchy . toNSView)
  return $ View d f ss

-}
