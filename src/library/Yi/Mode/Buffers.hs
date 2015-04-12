{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Buffers
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A minimalist emulation of emacs buffer menu mode, to be fleshed out later

module Yi.Mode.Buffers (listBuffers) where

import           Control.Applicative ((<$>))
import           Control.Category    ((>>>))
import           Control.Lens        (assign, (%~), (.~))
import           Data.List.NonEmpty  (toList)
import qualified Data.Text           as T (intercalate, pack)
import           System.FilePath     (takeFileName)
import           Yi.Buffer
import           Yi.Editor
import           Yi.Keymap           (Keymap, YiM, topKeymapA)
import           Yi.Keymap.Keys
import qualified Yi.Rope             as R (fromText, toString)

-- | Retrieve buffer list and open a them in buffer mode using the
-- 'bufferKeymap'.
listBuffers :: YiM ()
listBuffers = do
  withEditor $ do
    bs <- toList <$> getBufferStack
    let bufferList = R.fromText . T.intercalate "\n" $ map identString bs
    bufRef <- stringToNewBuffer (MemBuffer "Buffer List") bufferList
    switchToBufferE bufRef
  withCurrentBuffer $ do
    modifyMode $ modeKeymapA .~ topKeymapA %~ bufferKeymap
                 >>> modeNameA .~ "buffers"
    assign readOnlyA True

-- | Switch to the buffer with name at current name. If it it starts
-- with a @/@ then assume it's a file and try to open it that way.
switch :: YiM ()
switch = do
  -- the YiString -> FilePath -> Text conversion sucks
  s <- R.toString <$> withCurrentBuffer readLnB
  let short = T.pack $ if take 1 s == "/" then takeFileName s else s
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
  , oneOf [ char 'n', char ' ' ]    >>! lineDown
  , oneOf [ spec KEnter, char 'f' ] >>! (switch >> setReadOnly False)
  , char 'v'                        ?>>! (switch >> setReadOnly True)
  ]
  where
    setReadOnly = withCurrentBuffer . assign readOnlyA
