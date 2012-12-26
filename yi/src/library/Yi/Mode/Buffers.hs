-- Copyright (c) 2007, 2008 Stéphane "cognominal" Payrard
-- A minimalist emulation of emacs buffer menu mode, to be fleshed out later

module Yi.Mode.Buffers (
         listBuffers
) where

import Yi.Core
import Data.List ( intercalate )
import System.FilePath ( takeFileName )


listBuffers :: YiM  () 
listBuffers = do
     withEditor $  do
       bs <- getBufferStack
       bufRef <- stringToNewBuffer (Left "Buffer List")  $ fromString $ intercalate "\n" $ map identString bs
       switchToBufferE bufRef
     withBuffer $ do
       modifyMode $ \m -> m {modeKeymap = topKeymapA ^: bufferKeymap, modeName = "buffers"}
       putA readOnlyA True
switch :: YiM ()
switch =    do
    s <- withBuffer readLnB
    let short =  if take 1 s == "/"  then takeFileName s else s
    withEditor $ switchToBufferWithNameE short


bufferKeymap :: Keymap -> Keymap
bufferKeymap = do 
    (choice [
             char 'p'                         ?>>! lineUp,
             oneOf [char 'n', char ' ']       >>! lineDown,
             oneOf [ spec KEnter, char 'f' ]  >>! (switch >> ( withBuffer $ putA readOnlyA False)),
             char 'v'                        ?>>! switch >> ( withBuffer $ putA readOnlyA True)  ] 
     <||)

