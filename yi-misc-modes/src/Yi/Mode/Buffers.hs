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
import           Control.Monad       (forM_, mapM_, unless, when
                                     ,replicateM_)
import           Data.Functor        (void)
import           Lens.Micro.Platform ((.=), (%~), (.~), (%=), to, use)
import           Data.List.NonEmpty  (NonEmpty( (:|) ))
import qualified Data.Text           as T (intercalate, pack, append, Text
                                          , justifyLeft)
import           Yi.Buffer
import           Yi.Editor
import           Yi.Types            ( FBuffer(attributes)
                                     , Attributes(readOnly))
import           Yi.Buffer.Misc      (isUnchangedBuffer, replaceCharB
                                     ,gotoLnFrom, rightB )
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

-- | Add Emacs-like properties to the list of buffers. Currently the
--   flags .%* means that . is the current buffer, % that is read only and *
--   that has been modified.
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
           `T.append` fourSpaceRef ref `T.append` " "
           `T.append` identString buf

    fourSpaceRef :: BufferRef -> T.Text
    fourSpaceRef (BufferRef i) = T.justifyLeft 4 ' ' (T.pack (show i))


-- | Auxiliar function to extract the `BufferRef` from the format used in
--   this mode
extractBufferRef :: String -> BufferRef
extractBufferRef l = let ref' = read $ words (drop 4 l) !! 0
                     in  BufferRef ref'

-- | Switch to the buffer with name at current name. If it it starts
-- with a @/@ then assume it's a file and try to open it that way.
switch :: YiM ()
switch = do
  s <- R.toString <$> withCurrentBuffer readLnB
  let ref = extractBufferRef s
  withEditor $ switchToBufferE ref

-- | `setFlag offset char move` puts on the current line at the given
--   `offset` from the beginning the character `char` and depending on move
--   whether we move down a line. This is a general template for all the
--   setting moves on the list buffer.
setFlag :: Int -> Char -> Bool -> BufferM ()
setFlag n c move = do
  readOnlyA .= False
  moveToSol >> replicateM_ n rightB >> replaceCharB c
  readOnlyA .= True
  isLast <- atLastLine
  unless (isLast || not move) (void (gotoLnFrom 1))

setDeleteFlag,unsetDeleteFlag :: BufferM ()
setDeleteFlag = setFlag 0 'D' True
unsetDeleteFlag = setFlag 0 ' ' True

-- | Alternate the readonly status of a buffer on the list.
flipROFlag :: EditorM ()
flipROFlag = do
  s <- R.toString <$> withCurrentBuffer readLnB
  let ref = extractBufferRef s
      symbol = if s !! 1 == ' ' then '%' else ' '
  withGivenBuffer ref (readOnlyA %= not)
  withCurrentBuffer (setFlag 1 symbol False)

-- | eXecute the flagged for deletion files on the list buffer. Then
--   refresh the list.
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
-- __x__              → delete files flagged for deletion
-- __%__              → set/unset files as read-only
-- @
bufferKeymap :: Keymap -> Keymap
bufferKeymap = important $ choice
  [ char 'p'                        ?>>! lineUp
  , oneOf [ char 'n', char ' ' ]    >>! lineDown
  , oneOf [ spec KEnter, char 'f' ] >>! switch
  , char 'v'                        ?>>! (switch >> setReadOnly True)
  , char 'g'                        ?>>! listBuffers
  , char 'd'                        ?>>! setDeleteFlag
  , char 'x'                        ?>>! executeDelete
  , char 'u'                        ?>>! unsetDeleteFlag
  , char '%'                        ?>>! flipROFlag
  ]
  where
    setReadOnly = withCurrentBuffer . (.=) readOnlyA
