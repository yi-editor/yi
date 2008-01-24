-- Copyright (c) 2005,8,8 Jean-Philippe Bernardy

module Yi.Keymap.Emacs.KillRing where

import Yi.Core
import Yi.Keymap.Emacs.UnivArgument
import Yi.Buffer.Region
import Yi.Keymap
import Yi.Buffer
import Yi.Buffer.HighLevel
import Data.Dynamic
import Yi.Accessor
import Yi.Editor
import Control.Monad ( when, replicateM_ )

-- * Killring structure

data Killring = Killring { krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: [String]
                         , krLastYank :: Bool
                         }
    deriving (Typeable, Show)

instance Initializable Killring where
    initial = Killring { krKilled = False
                       , krAccumulate = False
                       , krContents = [[]]
                       , krLastYank = False
                       }

-- * Killring "ADT"

killringA :: Accessor Editor Killring
killringA = dynamicValueA .> dynamicA

maxDepth :: Int
maxDepth = 10

-- | Finish an atomic command, for the purpose of killring accumulation.
krEndCmd :: Killring -> Killring
krEndCmd kr@Killring {krKilled = killed} = kr {krKilled = False, krAccumulate = killed }

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
krPut :: String -> Killring -> Killring
krPut s kr@Killring {krContents = r@(x:xs), krAccumulate=acc}
    = kr {krKilled = True,
          krContents = if acc then (x++s):xs
                              else s:take maxDepth r }
krPut _ _ = error "killring invariant violated"

-- * Killring actions

--- | C-w
killRegionE :: YiM ()
killRegionE = do r <- withBuffer getSelectRegionB
                 text <- withBuffer $ readRegionB r
                 killringPut text
                 withBuffer unsetMarkB
                 withBuffer $ deleteRegionB r

-- | C-k
killLineE :: YiM ()
killLineE = withUnivArg $ \a -> case a of
               Nothing -> killRestOfLineE
               Just n -> replicateM_ (2*n) killRestOfLineE

killringPut :: String -> YiM ()
killringPut s = withEditor $ modifyA killringA $ krPut s

-- | Kill the rest of line
killRestOfLineE :: YiM ()
killRestOfLineE =
    do eol <- withBuffer atEol
       l <- withBuffer readRestOfLnB
       killringPut l
       withBuffer deleteToEol
       when eol $
            do c <- withBuffer readB
               killringPut [c]
               withBuffer (deleteN 1)

-- | C-y
yankE :: EditorM ()
yankE = do (text:_) <- getsA killringA krContents
           withBuffer0 $ do pointB >>= setSelectionMarkPointB
                            insertN text
                            unsetMarkB

-- | M-w
killRingSaveE :: YiM ()
killRingSaveE = do text <- withBuffer (readRegionB =<< getSelectRegionB)
                   killringPut text
                   withBuffer unsetMarkB
-- | M-y

-- TODO: Handle argument, verify last command was a yank
yankPopE :: EditorM ()
yankPopE = do withBuffer0 (deleteRegionB =<< getSelectRegionB)
              modifyA killringA $ \kr ->
                  let ring = krContents kr
                  in kr {krContents = tail ring ++ [head ring]}
              yankE

-- | C-M-w
appendNextKillE :: YiM ()
appendNextKillE = withEditor $ modifyA killringA (\kr -> kr {krKilled=True})
