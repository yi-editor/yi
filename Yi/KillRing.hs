-- Copyright (c) 2005,8 Jean-Philippe Bernardy
module Yi.KillRing (Killring
                   ,krKilled
                   ,krContents
                   ,krEndCmd
                   ,krPut
                   ,krSet, krGet
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

-- | Set the top of the killring. Never accumulate the previous content.
krSet :: String -> Killring -> Killring
krSet s kr@Killring {krContents = _:xs} = kr {krContents = s:xs}
krSet _ _ = error "killring invariant violated"

-- | Get the top of the killring.
krGet :: Killring -> String
krGet = head . krContents
