{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yi.Intero
-- Description : Intero support for Yi.
-- Copyright   : 2017 Jaro Reinders
-- License     : GPL-2
-- Maintainer  : yi-devel@googlegroups.com
--
-- This module implements actual Yi Actions using the hidden InteroAPI module.

module Yi.Intero (
    Intero
  , interoStart
  , interoLocAt, interoTypeAt, interoEval, interoUses
  ) where

import qualified Yi.Rope as R
import Control.Concurrent.MVar
import Data.Text (Text, unpack, pack)
import Data.Binary
import Yi.Types
import Yi.Region
import Data.Default (Default, def)
import Yi.Editor
import Control.Monad
import Yi.Buffer.Misc
import Control.Monad.Base
import Yi.Buffer.Region
import Yi.Buffer.Normal
import Yi.Core
import Yi.Monad
import Data.Semigroup ((<>))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Data.Text (pack)
import Control.Exception (IOException, try)

import qualified InteroAPI as Intero
import InteroAPI (Request)

-- | The main type that facilitates the communication with the background intero process.
-- Should be Nothing when intero is not yet started and Just an MVar that you can use
-- to make requests if intero is started.
--
-- This type is a YiVariable so we can use putEditorDyn when we start intero
-- and getEditorDyn whenever we need it.
newtype Intero = Intero { unIntero :: (Maybe (MVar Request)) }

instance Show Intero where
  show _ = "Intero"
instance Binary Intero where
  put _ = return ()
  get = return def
instance Default Intero where
  def = Intero Nothing
instance YiVariable Intero

-- | Start the Intero background process.
interoStart :: Action
interoStart = YiA $ do
  Intero intero <- getEditorDyn :: YiM Intero
  case intero of
    -- FIXME: change "." into the correct directory
    Nothing -> liftBase (try (Intero.start ".") :: IO (Either IOException (MVar Request)))
           >>= either (errorEditor . pack . show) (putEditorDyn . Intero . Just)
    Just _ -> errorEditor "Intero is already running"

-- | Pass a raw string to intero. Intero is like ghci in that you can
-- evaluate any haskell expression, but you can also use commands like
-- ":show modules" or ":show imports" like you would use them in ghci.
-- On top of all that Intero has implemented ide-like commands, like
-- ":type-at" and ":uses". Intero's github page has a small list of
-- features:
-- https://github.com/commercialhaskell/intero/blob/28271d50ca65c460cd0983cea13a2c4509b95583/TOOLING.md
interoEval :: String -> Action
interoEval command = YiA $
  either errorEditor return =<< runExceptT (do
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    res    <- liftBase $ Intero.eval intero command
    -- FIXME: close previous *intero* buffers before opening a new one.
    lift $ inNewBuffer "*intero*" (R.fromString res))

-- TODO: add possibility of using the current selection as input string to the intero command

-- | Intero's 'loc-at', 'type-at' and 'uses' commands all use the same file-range-name format
-- so I wrote a function to reduce code duplication.
interoFileRangeNameAction
  :: (MVar Request -> String -> (Int,Int,Int,Int) -> String -> IO String)
  -> Action
interoFileRangeNameAction f = YiA $
  either errorEditor return =<< runExceptT (do
    file   <- ExceptT $ maybe (Left "Not in file") Right <$> withCurrentBuffer (gets file)
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    region <- lift $ withCurrentBuffer (regionOfB unitWord)
    range  <- lift $ regionToRange region
    name   <- lift $ withCurrentBuffer (readRegionB region)
    res    <- liftBase (f intero file range (R.toString name))
    lift $ inNewBuffer "*intero*" (R.fromString res))

-- | Finds the location of the definition of the current word under the cursor.
-- This will open the result (just a string containing the module name) in a split window.
interoLocAt  :: Action
interoLocAt  = interoFileRangeNameAction Intero.locAt

-- | Finds the places where the current word under the cursor is used if it is a definition
-- or the place where it is defined if it isn't.
interoUses   :: Action
interoUses   = interoFileRangeNameAction Intero.uses

-- | Find the type of the current word under the cursor.
interoTypeAt :: Action
interoTypeAt = interoFileRangeNameAction Intero.typeAt

-- | Convert a Yi Region to Intero's range format.
regionToRange :: Region -> YiM (Int,Int,Int,Int)
regionToRange region = withCurrentBuffer $ do
  line  <- lineOf $ regionStart region
  col   <- colOf  $ regionStart region
  line' <- lineOf $ regionEnd   region
  col'  <- colOf  $ regionEnd   region
  return (line,col,line',col')

-- | Show a YiString in a split window.
inNewBuffer :: Text -> R.YiString -> YiM ()
inNewBuffer bufName content = withEditor $ withOtherWindow $ do
  void $ newBufferE (MemBuffer bufName) $ content