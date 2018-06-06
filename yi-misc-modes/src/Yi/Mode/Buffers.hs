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
import           Control.Monad       (forM_, mapM_, unless, when)
import           Data.Functor        (void)
import           Lens.Micro.Platform ((.=), (%~), (.~), to, use)
import           Data.List.NonEmpty  (NonEmpty( (:|) ))
import qualified Data.Text           as T (intercalate, pack, append, Text)
import           Yi.Buffer
import           Yi.Editor
import           Yi.Types            ( FBuffer(attributes)
                                     , Attributes(readOnly))
import           Yi.Buffer.Misc      (isUnchangedBuffer, replaceCharB
                                     , gotoLnFrom )
import           Yi.Buffer.HighLevel (moveToSol, lineStreamB, topB)
import           Yi.Buffer.Basic      (Direction(..), BufferRef(..))
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
    --
    br <- use (to bufferStack)
    bs <- getBufferStack
    let bufferList = R.fromText . T.intercalate "\n" $ bufferProperties br bs
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
bufferProperties :: NonEmpty BufferRef -> NonEmpty FBuffer -> [T.Text]
bufferProperties (curRef :| extraRef) (curBuf :| extraBufs) =
  characterize '.' curRef curBuf
  : map (uncurry (characterize ' ')) (zip extraRef extraBufs)
  where
    characterize :: Char -> BufferRef -> FBuffer -> T.Text
    characterize cur ref buf | attr <- attributes buf =
        let roChar = if readOnly attr then '%' else ' '
            modChar = if isUnchangedBuffer buf then ' ' else '*'
        in T.pack [cur, roChar, modChar, ' ']
           `T.append` threeSpaceRef ref `T.append` " "
           `T.append` identString buf

    threeSpaceRef :: BufferRef -> T.Text
    threeSpaceRef (BufferRef i)
      | i < 10 = ti `T.append` "   "
      | i >= 10 && i < 100 = ti `T.append` "  "
      | i >= 100 && i < 1000 = ti `T.append` " "
      | i >= 1000 && i < 10000 = ti
      | otherwise = error "Too many buffersRefs"
      where
        ti = T.pack (show i)

-- | Switch to the buffer with name at current name. If it it starts
-- with a @/@ then assume it's a file and try to open it that way.
switch :: YiM ()
switch = do
  s <- (drop 4 . R.toString) <$> withCurrentBuffer readLnB
  let ref = read $ (words s) !! 0
  withEditor $ switchToBufferE (BufferRef ref)

-- `setFlag ' '` is basically unsetting, so we make it general.
setFlag :: Char -> BufferM ()
setFlag c = do
  readOnlyA .= False
  moveToSol >> replaceCharB c
  readOnlyA .= True
  isLast <- atLastLine
  unless isLast (void (gotoLnFrom 1))

setDeleteFlag,unsetDeleteFlag :: BufferM ()
setDeleteFlag = setFlag 'D'
unsetDeleteFlag = setFlag ' '

executeDelete :: YiM ()
executeDelete = do
  bLines <- withCurrentBuffer (topB *> lineStreamB Forward)
  forM_ bLines $ \l ->
      let (flags, rest) = splitAt 4 (R.toString l)
          d:_ = flags
          ref' = read (words rest !! 0)
          ref = BufferRef ref'
      in when (d == 'D') (deleteBuffer ref)
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
  , char 'u'                        ?>>! unsetDeleteFlag
  ]
  where
    setReadOnly = withCurrentBuffer . (.=) readOnlyA
