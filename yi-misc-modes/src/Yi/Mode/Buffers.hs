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
import           Control.Monad       (forM_, mapM_, unless)
import           Data.Functor        (void)
import           Lens.Micro.Platform ((.=), (%~), (.~), to, use)
import           Data.List.NonEmpty  (NonEmpty( (:|) ))
import qualified Data.Text           as T (intercalate, pack, append, Text)
import           System.FilePath     (takeFileName)
import           Yi.Buffer
import           Yi.Editor
import           Yi.Types            (FBuffer(attributes), Attributes(readOnly))
import           Yi.Buffer.Misc      (isUnchangedBuffer, replaceCharB
                                     , gotoLnFrom )
import           Yi.Buffer.HighLevel (moveToSol, lineStreamB, topB)
import           Yi.Buffer.Basic (Direction(..))
import           Yi.Keymap           (Keymap, YiM, topKeymapA)
import           Yi.Keymap.Keys
import qualified Yi.Rope             as R (fromText, toString)

-- | Retrieve buffer list and open a them in buffer mode using the
-- 'bufferKeymap'.
listBuffers :: YiM ()
listBuffers = do
  withEditor $ do
    -- Delete previous lists of buffers. TODO: what happens with a
    -- buffer named *Buffer List*?
    use (to (findBufferWithName listBuffersName)) >>= mapM_ deleteBuffer
    bs <- getBufferStack
    let bufferList = R.fromText . T.intercalate "\n" $ bufferProperties bs
    bufRef <- stringToNewBuffer (MemBuffer listBuffersName) bufferList
    switchToBufferE bufRef
  withCurrentBuffer $ do
    modifyMode $ modeKeymapA .~ topKeymapA %~ bufferKeymap
                 >>> modeNameA .~ "buffers"
    readOnlyA .= True
  where
    listBuffersName :: T.Text
    listBuffersName = "Buffer List"

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

setDeleteFlag :: BufferM ()
setDeleteFlag = do
  readOnlyA .= False
  moveToSol >> replaceCharB 'D'
  readOnlyA .= True
  isLast <- atLastLine
  unless isLast (void (gotoLnFrom 1))

executeDelete :: YiM ()
executeDelete = do
  bLines <- withCurrentBuffer (topB *> lineStreamB Forward)
  forM_ bLines $ \l -> do
    let (d:_:_:_:name) = R.toString l
        short = T.pack $ if take 1 name == "/"
                           then takeFileName name
                           else name
    if d == 'D'
      then do use (to (findBufferWithName short)) >>= mapM_ deleteBuffer
      else return ()
  listBuffers

-- | Keymap for the buffer mode.
--
-- @
-- __p__              → line up
-- __n__ or __SPACE__ → line down
-- __ENTER__ or __f__ → open buffer
-- __v__              → open buffer as read-only
-- __g__              → reload buffer list
-- __d__              → flag this file for deletion
-- __g__              → delete files flagged for deletion
-- @
bufferKeymap :: Keymap -> Keymap
bufferKeymap = important $ choice
  [ char 'p'                        ?>>! lineUp
  , oneOf [ char 'n', char ' ' ]    >>! lineDown
  , oneOf [ spec KEnter, char 'f' ] >>! (switch >> setReadOnly False)
  , char 'v'                        ?>>! (switch >> setReadOnly True)
  , char 'g'                        ?>>! listBuffers
  , char 'd'                        ?>>! setDeleteFlag
  , char 'x'                        ?>>! executeDelete
  ]
  where
    setReadOnly = withCurrentBuffer . (.=) readOnlyA
