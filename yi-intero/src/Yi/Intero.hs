{-# LANGUAGE OverloadedStrings #-}
module Yi.Intero where

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

import qualified InteroAPI as Intero
import InteroAPI (Intero (Intero), Request)

instance Binary Intero where
  put _ = return ()
  get = return def
instance Default Intero where
  def = Intero Nothing
instance YiVariable Intero

interoStart :: Action
interoStart = YiA $ do
  Intero intero <- getEditorDyn :: YiM Intero
  case intero of
    -- TODO: change "." into actual directory
    Nothing -> liftBase (Intero.start ".") >>= putEditorDyn . Intero . Just
    Just _ -> errorEditor "intero-start: Already started"

interoEval :: String -> Action
interoEval command = YiA $ do
  Intero intero <- getEditorDyn :: YiM Intero
  case intero of
    Nothing -> errorEditor "intero-eval: Intero not initialized"
    Just req -> do
      res <- liftBase $ Intero.eval req command
      inNewBuffer "*intero*" (R.fromString res)

interoFileRangeNameAction
  :: Text
  -> (MVar Request -> String -> (Int,Int,Int,Int) -> String -> IO String)
  -> Action
interoFileRangeNameAction name f = YiA $ do
  mayFile <- withCurrentBuffer $ gets file
  case mayFile of
    Nothing -> errorEditor (name <> ": Not in file")
    Just file -> do
      Intero mayIntero <- getEditorDyn :: YiM Intero
      case mayIntero of
        Nothing -> errorEditor (name <> ": Intero not initialized")
        Just intero -> do
          region <- withCurrentBuffer $ regionOfB unitWord
          range <- regionToRange region
          name <- withCurrentBuffer $ readRegionB region
          res <- liftBase $ f intero file range (R.toString name)
          inNewBuffer "*intero*" (R.fromString res)

interoLocAt,interoUses,interoTypeAt :: Action
interoLocAt  = interoFileRangeNameAction "intero-loc-at"  Intero.locAt
interoUses   = interoFileRangeNameAction "intero-uses"    Intero.uses
interoTypeAt = interoFileRangeNameAction "intero-type-at" Intero.typeAt

regionToRange :: Region -> YiM (Int,Int,Int,Int)
regionToRange region = withCurrentBuffer $ do
  line  <- lineOf $ regionStart region
  col   <- colOf  $ regionStart region
  line' <- lineOf $ regionEnd   region
  col'  <- colOf  $ regionEnd   region
  return (line,col,line',col')

inNewBuffer :: Text -> R.YiString -> YiM ()
inNewBuffer bufName content = withEditor $ withOtherWindow $ do
  void $ newBufferE (MemBuffer bufName) $ content