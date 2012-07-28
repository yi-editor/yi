{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Yi.Keymap.Vim.TagStack(VimTagStack(..),
                              getTagStack,
                              setTagStack,
                              listTagStack,
                              pushTagStack,
                              peekTagStack,
                              popTagStack)
where

import System.FilePath(FilePath)
import Yi.Buffer.Basic(Point)
import Yi.Prelude(Initializable(..))
import Yi.Dynamic
import Yi.Editor

import Data.Binary
import Data.Typeable
{- 
import {-# source #-} Yi.Boot
import Yi.Core
import Yi.File
import Yi.History
import Yi.MiniBuffer
 -}

newtype VimTagStack = VimTagStack { tagsStack :: [(FilePath, Point)] }
    deriving (Typeable, Binary)

instance Initializable VimTagStack where
    initial = VimTagStack []

instance YiVariable VimTagStack

getTagStack :: EditorM VimTagStack
getTagStack = getDynamic

setTagStack :: VimTagStack -> EditorM ()
setTagStack = setDynamic

listTagStack :: EditorM [(FilePath, Point)]
listTagStack = return . tagsStack =<< getTagStack

pushTagStack :: FilePath -> Point -> EditorM ()
pushTagStack fp p = do VimTagStack ts <- getTagStack
                       setTagStack $ VimTagStack $ (fp, p):ts

peekTagStack :: EditorM (Maybe (FilePath, Point))
peekTagStack = do VimTagStack ts <- getTagStack
                  case ts of
                    []    -> return Nothing
                    (p:_) -> return $ Just p

-- pop 'count' element from the tag stack.
popTagStack :: Int -> EditorM (Maybe (FilePath, Point))
popTagStack count = do VimTagStack ts <- getTagStack
                       case drop (count - 1) ts of
                         []     -> return Nothing
                         (p:ps) -> do setTagStack $ VimTagStack ps
                                      return $ Just p
