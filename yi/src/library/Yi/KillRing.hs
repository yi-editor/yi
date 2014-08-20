{-# LANGUAGE TemplateHaskell, CPP, StandaloneDeriving, DeriveGeneric #-}

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

import Data.Binary
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif

import Data.List.NonEmpty hiding (length, drop)
import Prelude hiding (head, tail, take)
import Yi.Buffer.Basic


data Killring = Killring { krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: NonEmpty String
                         , krLastYank :: Bool
                         }
    deriving (Show)

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Killring)
#else
deriving instance Generic Killring
instance Binary a => Binary (NonEmpty a)
instance Binary Killring
#endif

maxDepth :: Int
maxDepth = 10

krEmpty :: Killring
krEmpty = Killring { krKilled = False
                   , krAccumulate = False
                   , krContents = "" :| []
                   , krLastYank = False
                   }


-- | Finish an atomic command, for the purpose of killring accumulation.
krEndCmd :: Killring -> Killring
krEndCmd kr@Killring {krKilled = killed} = kr {krKilled = False, krAccumulate = killed }

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
krPut :: Direction -> String -> Killring -> Killring
krPut dir s kr@Killring {krContents = r@(x :| xs), krAccumulate=acc}
    = kr {krKilled = True,
          krContents = if acc then (case dir of
                                      Forward  -> x++s
                                      Backward -> s++x) :| xs
                              else push s r}

-- | Push a string in the killring.
push :: String -> NonEmpty String -> NonEmpty String
--push s [] = [s]
push s r@(h:|t) = s :| if length h <= 1 then t else take maxDepth r
-- Don't save very small cutted text portions.

-- | Set the top of the killring. Never accumulate the previous content.
krSet :: String -> Killring -> Killring
krSet s kr@Killring {krContents = _ :| xs} = kr {krContents = s :| xs}

-- | Get the top of the killring.
krGet :: Killring -> String
krGet = head . krContents
