-- :buffers or :ls ex command to list buffers. 
module Yi.Keymap.Vim2.Ex.Commands.Buffers
    ( parse
    ) where

import Prelude ()
import Data.List 
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.Buffer.Misc
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

import qualified Data.Rope as R

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "buffers") <|> P.try ( P.string "ls") <|> P.try ( P.string "files" )
    bufName <- P.many P.anyChar
    return $ Common.pureExCommand {
        cmdShow = "buffers"
      , cmdAction = EditorA $ withEditor printBuffers 
      }



-- printBuffers :: EditorM ()
printBuffers = do 
    bufs <- getBufferStack
    -- printMsgs $ fmap show  bufs
    -- printMsg . intercalate "\n" $ fmap (show . view identA) bufs
    -- TODO shorten this string perhaps.
    -- TODO Add more information: buffer number, modified status, line number.
    -- TODO Do we already have a buffer type window, similar to dired?
    let bufIdents = fmap (show . view identA) bufs
    if length bufIdents > 1
      then withEditor . void $
             newBufferE (Left "Buffer list")
                        (R.fromString $ intercalate "\n" bufIdents)
      else printMsgs bufIdents 