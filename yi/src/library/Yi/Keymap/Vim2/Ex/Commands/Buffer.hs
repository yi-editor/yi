-- :buffer ex command to switch to named or numbered buffer.
module Yi.Keymap.Vim2.Ex.Commands.Buffer
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import Data.Maybe 
import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.Buffer.Misc
import Yi.Buffer.Basic
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    -- TODO bufIdent and the spaces should be optional, in which
    --      case the command becomes a noop just like for the empty string.
    -- TODO If bufIdent is a number the spaces should be optional.
    -- TODO If no bufIdent is given and the buffer is modified and 'hidden'
    --      is not set, should this operation be a noop?
    void $ P.try ( P.string "buffer") <|>
           P.try ( P.string "buf")    <|>
           P.try ( P.string "bu")     <|>
           P.try ( P.string "b")
    bufIdent <- P.many1 P.space *> P.many P.anyChar
    return $ Common.pureExCommand {
        cmdShow = "buffer"
      , cmdAction = EditorA $ switchToBuffer bufIdent 
      }


switchToBuffer :: String -> EditorM ()
switchToBuffer s = do
    case P.parse bufferRef "" s of
        Right ref -> switchByRef ref
        Left e    -> switchByName s
  where
    bufferRef = BufferRef . read <$> P.many1 P.digit


switchByName :: String -> EditorM ()
switchByName ""      = return ()
switchByName "#"     = switchToBufferWithNameE "" 
switchByName bufName = switchToBufferWithNameE bufName 


switchByRef :: BufferRef -> EditorM () 
switchByRef ref = do
    mBuf <- findBuffer ref
    maybe (return ()) (switchToBufferE . bkey) mBuf
