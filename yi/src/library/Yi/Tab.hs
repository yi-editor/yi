{-# LANGUAGE DeriveDataTypeable #-}

module Yi.Tab where

import Prelude()
import Yi.Prelude
import Data.Binary
import Data.Typeable
import qualified Data.List.PointedList as PL

import Yi.Window

type TabRef = Int

-- | A tab, containing a collection of windows
data Tab = Tab {
  tkey :: !TabRef, -- ^ For UI sync; fixes #304
  tabWindows :: !(PL.PointedList Window) -- ^ Visible windows
  }
 deriving Typeable

tabWindowsA :: Accessor Tab (PL.PointedList Window)
tabWindowsA = accessor tabWindows (\ws t -> t{tabWindows = ws})

instance Binary Tab where
  put (Tab tk ws) = put tk >> put ws
  get = Tab <$> get <*> get

instance Eq Tab where
  (==) t1 t2 = tkey t1 == tkey t2

instance Show Tab where
  show t = "Tab " ++ show (tkey t)

-- | A specialised version of "fmap".
mapWindows :: (Window -> Window) -> Tab -> Tab
mapWindows f (Tab tk ws) = Tab tk (fmap f ws)

-- | Forces all windows in the tab
forceTab :: Tab -> Tab
forceTab (Tab tk ws) = foldr seq (Tab tk ws) ws

-- | Folds over the windows in the tab
tabFoldl :: (a -> Window -> a) -> a -> Tab -> a
tabFoldl f z t = foldl f z (tabWindows t)

