{-# LANGUAGE DeriveDataTypeable #-}

--
-- Copyright (c) 2008 JP Bernardy
--
--

module Yi.Window where
import Data.Typeable
import Yi.Buffer.Basic (BufferRef)

------------------------------------------------------------------------
-- | A window onto a buffer.

data Window = Window {
                      isMini    :: !Bool   -- ^ regular or mini window?
                     ,bufkey    :: !BufferRef -- ^ the buffer this window opens to
                     ,height    :: !Int    -- ^ height of the window (in number of lines displayed)
                     ,wkey      :: !Int    -- ^ identifier for the window (for UI sync)
                     }
        deriving Typeable
-- | Get the identification of a window.
winkey :: Window -> (Bool, BufferRef)
winkey w = (isMini w, bufkey w)

instance Show Window where
    show w = "Window to " ++ show (bufkey w) 
             -- ++ "{" ++ show (tospnt w) ++ "->" ++ show (bospnt w) ++ "}" 
             ++ "(" ++ show (height w) ++ ")"

{-
-- | Is a given point within tospnt / bospnt?
pointInWindow :: Point -> Window -> Bool
pointInWindow point win = tospnt win <= point && point <= bospnt win
-}

dummyWindowKey :: Int
dummyWindowKey = (-1)

-- | Return a "fake" window onto a buffer.
dummyWindow :: BufferRef -> Window
dummyWindow b = Window False b 0 dummyWindowKey

