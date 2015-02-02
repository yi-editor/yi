{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Window
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Operations on 'Window's, in the emacs sense of the word.

module Yi.Window where

import Control.Applicative (Applicative ((<*>)), (<$>))
import Data.Binary         (Binary (..))
import Data.Default        (Default (def))
import Data.Typeable       (Typeable)
import Yi.Buffer.Basic     (BufferRef, WindowRef)
import Yi.JumpList         (JumpList)
import Yi.Region           (Region, emptyRegion)
import Yi.Utils            (makeLensesWithSuffix)

------------------------------------------------------------------------
-- | A window onto a buffer.

data Window = Window
    { isMini    :: !Bool -- ^ regular or mini window?
    , bufkey    :: !BufferRef -- ^ the buffer this window opens to
    , bufAccessList :: ![BufferRef]
      -- ^ list of last accessed buffers (former bufKeys). Last
      -- accessed one is first element
    , height    :: !Int -- ^ height of the window (in number of screen
                       -- lines displayed)
    , width     :: !Int -- ^ width of the window (in number of chars)
    , winRegion :: !Region -- ^ view area. note that the top point is
                          -- also available as a buffer mark.
    , wkey      :: !WindowRef -- ^ identifier for the window (for UI sync)
    -- This is required for accurate scrolling.
    -- Scrolling depends on the actual number of buffer
    -- lines displayed. Line wrapping changes that number
    -- relative to the height so we can't use height for that
    -- purpose.
    , actualLines :: !Int
      -- ^ The actual number of buffer lines displayed. Taking into
      -- account line wrapping
    , jumpList :: !JumpList
    } deriving (Typeable)

makeLensesWithSuffix "A" ''Window

instance Binary Window where
    put (Window mini bk bl _w _h _rgn key lns jl) =
        put mini >> put bk >> put bl >> put key >> put lns >> put jl
    get = Window <$> get <*> get <*> get
                   <*> return 0 <*> return 0 <*> return emptyRegion
                   <*> get <*> get <*> get


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

-- | Return a "fake" window onto a buffer.
dummyWindow :: BufferRef -> Window
dummyWindow b = Window False b [] 0 0 emptyRegion def 0 Nothing
