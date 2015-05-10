{-# LANGUAGE CPP                #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.KillRing
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Killring operations.

module Yi.KillRing ( Killring
                   , _krKilled
                   , _krContents
                   , krKilled
                   , krContents
                   , krEndCmd
                   , krPut
                   , krSet, krGet
                   , krEmpty
                   , krLastYank
                   )
    where

import           Prelude             hiding (head, tail, take)

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens        (makeLenses, (^.))
import           Data.Binary         (Binary, get, put)
import           Data.List.NonEmpty  (NonEmpty (..), head, take)
import           Data.Monoid         (mempty, (<>))
import           Yi.Buffer.Basic     (Direction (..))
import qualified Yi.Rope             as R (YiString, length)


data Killring = Killring { _krKilled :: Bool
                         , _krAccumulate :: Bool
                         , _krContents :: NonEmpty R.YiString
                         , _krLastYank :: Bool
                         } deriving (Show, Eq)

instance Binary Killring where
  put (Killring k a c l) =
    let putNE (x :| xs) = put x >> put xs
    in put k >> put a >> putNE c >> put l
  get = let getNE = (:|) <$> get <*> get
        in Killring <$> get <*> get <*> getNE <*> get

makeLenses ''Killring

maxDepth :: Int
maxDepth = 10

krEmpty :: Killring
krEmpty = Killring { _krKilled = False
                   , _krAccumulate = False
                   , _krContents = mempty :| mempty
                   , _krLastYank = False
                   }

-- | Finish an atomic command, for the purpose of killring accumulation.
krEndCmd :: Killring -> Killring
krEndCmd kr = kr { _krKilled = False , _krAccumulate = kr ^. krKilled }

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
krPut :: Direction -> R.YiString -> Killring -> Killring
krPut dir s kr@Killring { _krContents = r@(x :| xs) }
    = kr { _krKilled = True
         , _krContents =
           if kr ^. krAccumulate
           then (case dir of Forward  -> x <> s
                             Backward -> s <> x) :| xs
           else push s r
         }

-- | Push a string in the killring.
push :: R.YiString -> NonEmpty R.YiString -> NonEmpty R.YiString
push s r@(h :| t) = s :| if R.length h <= 1 then t else take maxDepth r
-- Don't save very small cutted text portions.

-- | Set the top of the killring. Never accumulate the previous content.
krSet :: R.YiString -> Killring -> Killring
krSet s kr@Killring {_krContents = _ :| xs} = kr {_krContents = s :| xs}

-- | Get the top of the killring.
krGet :: Killring -> R.YiString
krGet = head . _krContents
