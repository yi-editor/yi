{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config.Users.Reiner
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Config.Users.Reiner (setup, main) where

import           Control.Lens
import           Data.Monoid
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           Yi.Command(buildRun)
import           Yi.Config.Simple
import qualified Yi.Mode.Haskell as Haskell
import           Yi.Mode.Latex(latexMode3)
import           Yi.Monad
import qualified Yi.Rope as R
import           Yi.Utils

-- | The main entry point, when used as a standalone yi.hs file.
main :: IO ()
main = configMain defaultEmacsConfig setup

-- | Registers my settings in the 'ConfigM' monad.
setup :: ConfigM ()
setup = do
  setFrontendPreferences ["pango", "vte", "vty"]
  fontSize .= Just 9

  globalBindKeys globalBindings
  evaluator .= publishedActionsEvaluator
  publishAction "createDirectory" yiCreateDirectory

  addMode Haskell.fastMode
  modeBindKeys Haskell.fastMode
    (ctrlCh 'c' ?>> ctrlCh 's' ?>>! insertHaskSection)

  -- LaTeX stuff
  addMode latexMode3
  publishAction "compileLatex" compileLatex
  modeBindKeys latexMode3 (ctrlCh 'c' ?>> ctrlCh 'c' ?>>! compileLatex)

--------------------------------------------------------------------------------
--                            Actions and bindings                            --
--------------------------------------------------------------------------------
yiCreateDirectory :: YiM ()
yiCreateDirectory = do
    BufferFileInfo{bufInfoFileName} <- withEditor $ withBuffer0 bufInfoB
    let dir = takeDirectory bufInfoFileName
    exists <- io $ doesDirectoryExist dir
    if not exists
    then do io $ createDirectoryIfMissing True dir
            withEditor $ printMsg $  "Created directory '" <> T.pack dir <> "'."
    else withEditor $ printMsg $ "Directory already exists!"

sectionSize :: Int
sectionSize = 80

-- inserts the "Actions and bindings" header above
insertHaskSection :: R.YiString -> BufferM ()
insertHaskSection s
   | lenS >= sectionSize - 6 = do
           insertDashes lenS
           newlineB
           insertN s
           newlineB
           insertDashes lenS
   | otherwise = do
           insertDashes sectionSize
           newlineB
           insertN "--"
           let nSpaces = sectionSize - 4 - lenS
               nSpacesL = nSpaces `div` 2
               nSpacesR = nSpaces - nSpacesL
           insertN $ repChar nSpacesL ' '
           insertN s
           insertN $ repChar nSpacesR ' '
           insertN "--"
           newlineB
           insertDashes sectionSize
  where lenS = R.length s
        insertDashes n = insertN $ repChar n '-'
        repChar n c = R.fromText . T.replicate n $ T.singleton c


globalBindings :: I Event Yi.Config.Simple.Action ()
globalBindings = choice
   [ ctrlCh '\t' ?>>! nextWinE
   , shift (ctrlCh '\t') ?>>! prevWinE
   , metaCh 'r' ?>>! reload
   ]

compileLatex :: YiM ()
compileLatex = do
  withEditor (withBuffer0 $ gets file) >>= \case
    Just filename -> buildRun "pdflatex" ["--file-line-error"
                                         , "--interaction=nonstopmode"
                                         , T.pack filename
                                         ] (const $ return ())
    Nothing -> return ()
