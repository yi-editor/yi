{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Buffers
-- Copyright   :  (c) Stéphane "cognominal" Payrard 2007-2008
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A minimalist emulation of emacs buffer menu mode, to be fleshed out later

module Yi.Mode.Buffers (listBuffers) where

import Control.Applicative ((<$>))
import Control.Lens
import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import Yi.OldRope (fromString)
import System.FilePath (takeFileName)
import Yi.Core

-- | Retrieve buffer list and open a them in buffer mode using the
-- 'bufferKeymap'.
listBuffers :: YiM  ()
listBuffers = do
  withEditor $  do
    bs <- toList <$> getBufferStack
    let bufferList = fromString . intercalate "\n" $ map identString bs
    bufRef <- stringToNewBuffer (Left "Buffer List") bufferList
    switchToBufferE bufRef
  withBuffer $ do
    modifyMode $ \m -> m & modeKeymapA .~ topKeymapA %~ bufferKeymap
                         & modeNameA .~ "buffers"
    assign readOnlyA True

-- | Switch to the buffer with name at current name. If it it starts
-- with a @/@ then assume it's a file and try to open it that way.
switch :: YiM ()
switch = do
  s <- withBuffer readLnB
  let short =  if take 1 s == "/"  then takeFileName s else s
  withEditor $ switchToBufferWithNameE short

-- | Keymap for the buffer mode.
--
-- @
-- __p__              → line up
-- __n__ or __SPACE__ → line down
-- __ENTER__ or __f__ → open buffer
-- __v__              → open buffer as read-only
-- @
bufferKeymap :: Keymap -> Keymap
bufferKeymap = important $ choice
  [ char 'p'                        ?>>! lineUp
  , oneOf [char 'n', char ' ']      >>! lineDown
  , oneOf [ spec KEnter, char 'f' ] >>! (switch >> setReadOnly False)
  , char 'v'                        ?>>! (switch >> setReadOnly True)
  ]
  where
    setReadOnly = withBuffer . assign readOnlyA
