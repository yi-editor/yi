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

import Control.Monad (ap)
import Data.Binary
import Yi.Buffer.Implementation (Direction(..))

data Killring = Killring { krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: [String]
                         , krLastYank :: Bool
                         }
    deriving (Show {-! Binary !-})

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
krPut :: Direction -> String -> Killring -> Killring
krPut dir s kr@Killring {krContents = r@(x:xs), krAccumulate=acc}
    = kr {krKilled = True,
          krContents = if acc then (case dir of 
                                      Forward  -> x++s
                                      Backward -> s++x):xs
                              else push s r}
krPut _ _ _ = error "killring invariant violated"

-- | Push a string in the killring.
push :: String -> [String] -> [String]
push s [] = [s]
push s r@(h:t) = s : if length h <= 1 then t else take maxDepth r 
-- Don't save very small cutted text portions.

-- | Set the top of the killring. Never accumulate the previous content.
krSet :: String -> Killring -> Killring
krSet s kr@Killring {krContents = _:xs} = kr {krContents = s:xs}
krSet _ _ = error "killring invariant violated"

-- | Get the top of the killring.
krGet :: Killring -> String
krGet = head . krContents



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 1998721949

instance Binary Killring
    where put (Killring x1
                        x2
                        x3
                        x4) = return () >> (put x1 >> (put x2 >> (put x3 >> put x4)))
          get = case 0 of
                    0 -> ap (ap (ap (ap (return Killring) get) get) get) get
