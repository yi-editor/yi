{-# LANGUAGE NoMonomorphismRestriction, NamedFieldPuns, DoAndIfThenElse #-}
module Yi.Config.Users.Reiner (setup, main) where

import Yi.Config.Simple
import Yi.Prelude hiding ((%=))
import qualified Prelude

import qualified Yi.Mode.Haskell as Haskell
import Yi.Mode.Latex(latexMode3)
import Yi.Command(buildRun)

import System.Directory
import System.FilePath

-- | The main entry point, when used as a standalone yi.hs file.
main = configMain defaultEmacsConfig setup

-- | Registers my settings in the 'ConfigM' monad.
setup :: ConfigM ()
setup = do
  setFrontendPreferences ["pango", "vte", "vty"]
  fontSize %= Just 9

  globalBindKeys globalBindings
  evaluator %= publishedActionsEvaluator
  publishAction "createDirectory" yiCreateDirectory

  addMode Haskell.fastMode
  modeBindKeys Haskell.fastMode (ctrlCh 'c' ?>> ctrlCh 's' ?>>! insertHaskSection)

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
    then do
            io $ createDirectoryIfMissing True dir
            withEditor $ printMsg $  "Created directory '" ++ dir ++ "'."
    else withEditor $ printMsg $ "Directory already exists!"

sectionSize = 80

-- inserts the "Actions and bindings" header above
insertHaskSection :: String -> BufferM ()
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
           insertN (replicate nSpacesL ' ')
           insertN s
           insertN (replicate nSpacesR ' ')
           insertN "--"
           newlineB
           insertDashes sectionSize
  where lenS = Prelude.length s
        insertDashes n = insertN (replicate n '-')
    

globalBindings = choice
   [ 
     ctrlCh '\t' ?>>! nextWinE,
     shift (ctrlCh '\t') ?>>! prevWinE,
     metaCh 'r' ?>>! reload
   ]

compileLatex = do
    mfilename <- withEditor $ withBuffer0 (gets file)
    case mfilename of
        Just filename -> buildRun "pdflatex" ["--file-line-error", "--interaction=nonstopmode", filename] (const $ return ())
        Nothing -> return ()
