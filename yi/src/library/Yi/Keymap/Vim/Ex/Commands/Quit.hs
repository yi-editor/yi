module Yi.Keymap.Vim.Ex.Commands.Quit
    ( parse
    ) where

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Foldable (find)

import qualified Text.ParserCombinators.Parsec as P

import Yi.Core (quitEditor, errorEditor, closeWindow)
import Yi.Buffer
import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Yi.Monad
import Yi.Window (bufkey)

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    ws <- P.many (P.char 'w')
    void $ P.try ( P.string "quit") <|> P.string "q"
    as <- P.many (P.try ( P.string "all") <|> P.string "a")
    bangs <- P.many (P.char '!')
    return $! quit (not $ null ws) (not $ null bangs) (not $ null as)

quit :: Bool -> Bool -> Bool -> ExCommand
quit w f a = Common.impureExCommand {
    cmdShow = concat
        [ if w then "w" else ""
        , "quit"
        , if a then "all" else ""
        , if f then "!" else ""
        ]
  , cmdAction = YiA $ action w f a
  }

action :: Bool -> Bool -> Bool -> YiM ()
action False False False = quitWindowE
action False False  True = quitAllE
action  True False False = viWrite >> closeWindow
action  True False  True = saveAndQuitAllE
action False  True False = closeWindow
action False  True  True = quitEditor
action  True  True False = viWrite >> closeWindow
action  True  True  True = saveAndQuitAllE

quitWindowE :: YiM ()
quitWindowE = do
    nw <- withBuffer needsAWindowB
    ws <- withEditor $ use currentWindowA >>= windowsOnBufferE . bufkey
    if length ws == 1 && nw
       then errorEditor "No write since last change (add ! to override)"
       else closeWindow

quitAllE :: YiM ()
quitAllE = do
    bs <- mapM (\b -> (,) b <$> withEditor (withGivenBuffer0 b needsAWindowB)) =<< readEditor bufferStack
    -- Vim only shows the first modified buffer in the error.
    case find snd bs of
        Nothing -> quitEditor
        Just (b, _) -> do
            bufferName <- withEditor $ withGivenBuffer0 b $ gets file
            errorEditor $ "No write since last change for buffer "
                        ++ show bufferName
                        ++ " (add ! to override)"

saveAndQuitAllE :: YiM ()
saveAndQuitAllE = Common.forAllBuffers fwriteBufferE >> quitEditor

needsAWindowB :: BufferM Bool
needsAWindowB = do
    isWorthless <- gets (either (const True) (const False) . (^. identA))
    canClose <- gets isUnchangedBuffer
    return (not (isWorthless || canClose))
