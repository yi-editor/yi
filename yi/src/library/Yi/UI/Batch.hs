
module Yi.UI.Batch (start) where

import Yi.Config    (UIBoot)
import Yi.UI.Common (dummyUI)

-- | Initialise the ui
start :: UIBoot
start _cfg _ch _outCh _ed = do
    mapM_ putStrLn ["Starting 'batch' UI...",
                    "Are you sure you compiled with support for any real UI?",
                    "(for example, pass -fvty to cabal install)"]
    return dummyUI

