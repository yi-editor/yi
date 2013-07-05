module Yi.Keymap.Vim2.Ex.Commands.Quit
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import Data.Either (either)

import qualified Text.ParserCombinators.Parsec as P

import Yi.Core (quitEditor, errorEditor, closeWindow)
import Yi.Buffer
import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common

commands :: [ExCommandBox]
commands =
    [ pack (Quit undefined undefined undefined)
    ]

-- | "q", "wq", "q!", ..
data Quit = Quit {
        _quitWrite :: !Bool
      , _quitForce :: !Bool
      , _quitAll :: !Bool
    }

instance Show Quit where
    show (Quit w f a) = (if w then "w" else "")
                     ++ "quit"
                     ++ (if a then "all" else "")
                     ++ (if f then "!" else "")

instance ExCommand Quit where
    cmdParse _ = parse $ do
        ws <- P.many (P.char 'w')
        discard $ P.try ( P.string "quit") <|> P.string "q"
        as <- P.many (P.try ( P.string "all") <|> P.string "a")
        bangs <- P.many (P.char '!')
        return $! Quit (not $ null ws) (not $ null bangs) (not $ null as)
    cmdAction = Right . action

action :: Quit -> YiM ()
action (Quit False False False) = closeWindow
action (Quit False False  True) = quitAllE
action (Quit  True False False) = viWrite >> closeWindow
action (Quit  True False  True) = saveAndQuitAllE
action (Quit False  True False) = quitEditor
action (Quit False  True  True) = closeWindow
action (Quit  True  True False) = viWrite >> closeWindow
action (Quit  True  True  True) = saveAndQuitAllE


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
saveAndQuitAllE = forAllBuffers fwriteBufferE >> quitEditor

needsAWindowB :: BufferM Bool
needsAWindowB = do
    isWorthless <- gets (either (const True) (const False) . (^. identA))
    canClose <- gets isUnchangedBuffer
    return (not (isWorthless || canClose))
