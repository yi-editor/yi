{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

--
-- Copyright (c) 2008 JP Bernardy
--
--

module Yi.Window where
import Data.Binary
import Data.Typeable
import Yi.Buffer.Basic (BufferRef)
import Yi.Region (Region,emptyRegion)
import Control.Applicative

------------------------------------------------------------------------
-- | A window onto a buffer.

type WindowRef = Int

data Window = Window {
                      isMini    :: !Bool   -- ^ regular or mini window?
                     ,bufkey    :: !BufferRef -- ^ the buffer this window opens to
                     ,bufAccessList :: ![BufferRef] -- ^ list of last accessed buffers (former bufKeys). Last accessed one is first element
                     ,height    :: Int    -- ^ height of the window (in number of lines displayed)
                     ,winRegion    :: Region -- ^ get view area
                     ,wkey      :: !WindowRef -- ^ identifier for the window (for UI sync)
                     }
        deriving (Typeable)

instance Binary Window where
    put (Window mini bk bl _h _rgn key) = put mini >> put bk >> put bl >> put key
    get = Window <$> get <*> get <*> get
                   <*> return 0 <*> return emptyRegion
                   <*> get


-- | Get the identification of a window.
winkey :: Window -> (Bool, BufferRef)
winkey w = (isMini w, bufkey w)

instance Show Window where
    show w = "Window to " ++ show (bufkey w) 
             -- ++ "{" ++ show (tospnt w) ++ "->" ++ show (bospnt w) ++ "}" 
             ++ "(" ++ show (height w) ++ ")"

instance Eq Window where
    (==) w1 w2 = wkey w1 == wkey w2

{-
-- | Is a given point within tospnt / bospnt?
pointInWindow :: Point -> Window -> Bool
pointInWindow point win = tospnt win <= point && point <= bospnt win
-}

dummyWindowKey :: Int
dummyWindowKey = (-1)

-- | Return a "fake" window onto a buffer.
dummyWindow :: BufferRef -> Window
dummyWindow b = Window False b [] 0 emptyRegion dummyWindowKey

