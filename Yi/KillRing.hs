{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright (c) 2005,8 Jean-Philippe Bernardy



module Yi.KillRing (Killring
                   ,krKilled
                   ,krContents
                   ,krEndCmd
                   ,krPut
                   ,krEmpty
                   ) 
    where


data Killring = Killring { krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: [String]
                         , krLastYank :: Bool
                         }
    deriving (Show)

maxDepth :: Int
maxDepth = 10

krEmpty :: Killring
krEmpty = Killring { krKilled = False
                   , krAccumulate = False
                   , krContents = [[]]
                   , krLastYank = False
                   }


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
