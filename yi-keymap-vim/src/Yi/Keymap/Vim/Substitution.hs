{-# language OverloadedStrings #-}

module Yi.Keymap.Vim.Substitution
    ( substituteE
    , substituteConfirmE
    , repeatSubstitutionE
    , repeatSubstitutionFlaglessE
    ) where

import Control.Monad (void)
import Data.Monoid
import Yi.MiniBuffer
import Yi.Keymap (Keymap)
import qualified Yi.Rope as R
import Yi.Regex
import Yi.Buffer
import Yi.Editor
import Yi.Search
import Yi.Keymap.Keys (char, choice, (?>>!))
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.StateUtils

substituteE :: Substitution -> BufferM Region -> EditorM ()
substituteE s@(Substitution from to global caseInsensitive confirm) regionB = do
        let opts = if caseInsensitive then [IgnoreCase] else []
        lines' <- withCurrentBuffer $ regionB >>= linesOfRegionB
        regex <- if R.null from
                    then getRegexE
                    else return . (either (const Nothing) Just) 
                            . makeSearchOptsM opts . R.toString $ from
        case regex of
            Nothing -> printMsg "No previous search pattern"
            Just regex' -> do
                saveSubstitutionE s
                if confirm
                then substituteConfirmE regex' to global lines'
                else do
                    withCurrentBuffer $ do
                        -- We need to reverse the lines' here so that replacing
                        -- does not effect the regions in question.
                        mapM_ (void . searchAndRepRegion0 regex' to global) (reverse lines')
                        moveToSol

-- | Run substitution in confirm mode
substituteConfirmE :: SearchExp -> R.YiString -> Bool -> [Region] -> EditorM ()
substituteConfirmE regex to global lines' = do
    -- TODO This highlights all matches, even in non-global mode
    -- and could potentially be classified as a bug. Fixing requires
    -- changing the regex highlighting api.
    setRegexE regex
    regions <- withCurrentBuffer $ findMatches regex global lines'
    substituteMatch to 0 False regions

-- | All matches to replace under given flags
findMatches :: SearchExp -> Bool -> [Region] -> BufferM [Region]
findMatches regex global lines' = do
    let f = if global then id else take 1
    concat <$> mapM (fmap f . regexRegionB regex) lines'

-- | Runs a list of matches using itself as a continuation
substituteMatch :: R.YiString -> Int -> Bool -> [Region] -> EditorM ()
substituteMatch _ _ _ [] = resetRegexE
substituteMatch to co autoAll (m:ms) = do
    let m' = offsetRegion co m
    withCurrentBuffer . moveTo $ regionStart m'
    len <- withCurrentBuffer $ R.length <$> readRegionB m'
    let diff = R.length to - len
        tex = "replace with " <> R.toText to <> " (y/n/a/q)?"
    if autoAll
        then do withCurrentBuffer $ replaceRegionB m' to
                substituteMatch to (co + diff) True ms
        else void . spawnMinibufferE tex . const $ askKeymap to co (co + diff) m ms

-- | Offsets a region (to account for a region prior being modified)
offsetRegion :: Int -> Region -> Region
offsetRegion k reg = mkRegion (regionStart reg + k') (regionEnd reg + k')
    where k' = fromIntegral k

-- | Actual choices during confirm mode.
askKeymap :: R.YiString -> Int -> Int -> Region -> [Region] -> Keymap
askKeymap to co co' m ms = choice [
      char 'n' ?>>! cleanUp >> substituteMatch to co False ms
    , char 'a' ?>>! do cleanUp
                       replace
                       substituteMatch to co' True ms
    , char 'y' ?>>! do cleanUp
                       replace
                       substituteMatch to co' False ms
    , char 'q' ?>>! cleanUp >> resetRegexE
    ]
    where cleanUp = closeBufferAndWindowE
          replace = withCurrentBuffer $ replaceRegionB (offsetRegion co m) to

repeatSubstitutionFlaglessE :: Substitution -> EditorM ()
repeatSubstitutionFlaglessE (Substitution from to _ _ _) =
    substituteE (Substitution from to False False False) (regionOfB Line)

repeatSubstitutionE :: Substitution -> EditorM ()
repeatSubstitutionE s = substituteE s (regionOfB Line)
