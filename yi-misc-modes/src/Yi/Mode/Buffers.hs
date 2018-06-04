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

import           Control.Category    ((>>>))
import           Lens.Micro.Platform ((.=), (%~), (.~))
import           Data.List.NonEmpty  (NonEmpty( (:|) ))
import qualified Data.Text           as T (intercalate, pack, append, Text)
import           System.FilePath     (takeFileName)
import           Yi.Buffer
import           Yi.Editor
import           Yi.Types            (FBuffer(attributes), Attributes(readOnly))
import           Yi.Buffer.Misc      (isUnchangedBuffer)
import           Yi.Keymap           (Keymap, YiM, topKeymapA)
import           Yi.Keymap.Keys
import qualified Yi.Rope             as R (fromText, toString)

-- | Retrieve buffer list and open a them in buffer mode using the
-- 'bufferKeymap'.
listBuffers :: YiM ()
listBuffers = do
  withEditor $ do
    bs <- getBufferStack
    let bufferList = R.fromText . T.intercalate "\n" $ bufferProperties bs
    bufRef <- stringToNewBuffer (MemBuffer "Buffer List") bufferList
    switchToBufferE bufRef
  withCurrentBuffer $ do
    modifyMode $ modeKeymapA .~ topKeymapA %~ bufferKeymap
                 >>> modeNameA .~ "buffers"
    readOnlyA .= True

-- | Add Emacs-like properties to the list of buffers.
bufferProperties :: NonEmpty FBuffer -> [T.Text]
bufferProperties (curBuf :| extraBufs) =
  characterize '.' curBuf : map (characterize ' ') extraBufs
  where
    characterize :: Char -> FBuffer -> T.Text
    characterize cur buf | attr <- attributes buf =
        let roChar = if readOnly attr then '%' else ' '
            modChar = if isUnchangedBuffer buf then ' ' else '*'
        in T.pack [cur, roChar, modChar, ' '] `T.append` identString buf

-- | Switch to the buffer with name at current name. If it it starts
-- with a @/@ then assume it's a file and try to open it that way.
switch :: YiM ()
switch = do
  -- the YiString -> FilePath -> Text conversion sucks
  s <- (drop 4 . R.toString) <$> withCurrentBuffer readLnB
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
    setReadOnly = withCurrentBuffer . (.=) readOnlyA
