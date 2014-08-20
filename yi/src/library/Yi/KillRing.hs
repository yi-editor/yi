{-# LANGUAGE TemplateHaskell, CPP, StandaloneDeriving, DeriveGeneric #-}

-- Copyright (c) 2005,8 Jean-Philippe Bernardy
module Yi.KillRing ( Killring
                   , _krKilled
                   , _krContents
                   , krKilled
                   , krContents
                   , krEndCmd
                   , krPut
                   , krSet, krGet
                   , krEmpty
                   )
    where

import Data.Binary
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif

import Control.Lens
import Data.List.NonEmpty hiding (length, drop)
import Prelude hiding (head, tail, take)
import Yi.Buffer.Basic


data Killring = Killring { _krKilled :: Bool
                         , _krAccumulate :: Bool
                         , _krContents :: NonEmpty String
                         , _krLastYank :: Bool
                         }
    deriving (Show)

makeLenses ''Killring

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Killring)
$(derive makeBinary ''NonEmpty)
#else
deriving instance Generic Killring
instance Binary a => Binary (NonEmpty a)
instance Binary Killring
#endif

maxDepth :: Int
maxDepth = 10

krEmpty :: Killring
krEmpty = Killring { _krKilled = False
                   , _krAccumulate = False
                   , _krContents = "" :| []
                   , _krLastYank = False
                   }

-- | Finish an atomic command, for the purpose of killring accumulation.
krEndCmd :: Killring -> Killring
krEndCmd kr = kr { _krKilled = False , _krAccumulate = kr ^. krKilled }

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
krPut :: Direction -> String -> Killring -> Killring
krPut dir s kr@Killring { _krContents = r@(x :| xs) }
    = kr { _krKilled = True
         , _krContents =
           if kr ^. krAccumulate
           then (case dir of { Forward -> x++s; Backward -> s++x }) :| xs
           else push s r
         }

-- | Push a string in the killring.
push :: String -> NonEmpty String -> NonEmpty String
push s r@(h :| t) = s :| if length h <= 1 then t else take maxDepth r
-- Don't save very small cutted text portions.

-- | Set the top of the killring. Never accumulate the previous content.
krSet :: String -> Killring -> Killring
krSet s kr@Killring {_krContents = _ :| xs} = kr {_krContents = s :| xs}

-- | Get the top of the killring.
krGet :: Killring -> String
krGet = head . _krContents
