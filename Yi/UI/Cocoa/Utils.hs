--
-- Copyright (c) 2008 Gustav Munkby
--

-- | Helper functions for the Cocoa frontend

module Yi.UI.Cocoa.Utils where

import Prelude hiding (init)
import Yi.Debug

import Foundation hiding (new)
import AppKit

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
