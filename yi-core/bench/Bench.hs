{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
module Main where

import           Control.DeepSeq
import           Control.Monad
import           Criterion
import           Criterion.Main hiding (defaultConfig)
import qualified Data.List as L
import           System.Environment
import           System.IO
import           Text.Printf (printf)
import           Yi.Buffer
import           Yi.Config (Config)
import           Yi.Config.Default (defaultConfig)
import           Yi.Editor
import qualified Yi.Rope as R

-- bogus instance
instance NFData Editor where
  rnf !_ = ()

data EditorAction = forall a b. (NFData a, NFData b) =>
  EditorAction
    { _ea_act :: b -> EditorM a
    , _ea_report :: a -> IO ()
    , _ea_setup :: EditorM b
    , _ea_name :: String
    , _ea_config :: Config
    }

simpleAction :: String -> EditorM () -> EditorAction
simpleAction n act = EditorAction
  { _ea_act = \() -> act
  , _ea_report = \() -> return ()
  , _ea_name = n
  , _ea_config = defaultConfig
  , _ea_setup = return ()
  }

insertN :: Int -> EditorAction
insertN !n = EditorAction
  { _ea_act = \() -> do
     runLoop
  , _ea_report = \yis_l ->
      putStrLn $ printf "Buffer has %d characters." yis_l

  , _ea_name = "insert" ++ show n
  , _ea_config = defaultConfig
  , _ea_setup = return ()
  }
  where
    spin n' | n' <= 0 = R.length <$> elemsB
            | otherwise = do
                insertB 'X'
                spin $! n' - 1
    runLoop = withCurrentBuffer $ spin n

acts :: [EditorAction]
acts = [ simpleAction "split20" $ replicateM_ 20 splitE
       , simpleAction "newTab20" (replicateM_ 20 newTabE)
       , Main.insertN 10
       , Main.insertN 100
       , Main.insertN 1000
       , Main.insertN 100000
       , Main.insertN 1000000
       ]

benchEditor :: (NFData a, NFData b)
            => String -- ^ Benchmark name
            -> Config -- ^ Config
            -> EditorM a -- ^ Setup
            -> (a -> EditorM b) -- ^ Action
            -> Benchmark
benchEditor bname c setup act =
  env (return $! runEditor c setup emptyEditor) $ \ ~(setupEditor, a) -> do
    bench bname $ nf (\a' -> snd $ runEditor c (act a') setupEditor) a

main :: IO ()
main = getArgs >>= \case
  ["list_actions"] -> print $ map _ea_name acts
  ["run_action", action_name] -> case L.find ((action_name ==) . _ea_name) acts of
    Just EditorAction{..} ->
      let !(!_, b) = runEditor _ea_config (_ea_setup >>= _ea_act) emptyEditor
      in do
        _ea_report b
        putStrLn $ _ea_name  ++ " finished."
    _ -> do
      hPutStrLn stderr $ "No such action: " ++ action_name
      hPutStrLn stderr $ "Available actions: " ++ show (map _ea_name acts)
  _ -> do
    let benchmarks :: [Benchmark]
        benchmarks = flip map acts $ \EditorAction{..} ->
          benchEditor _ea_name _ea_config _ea_setup _ea_act
    defaultMain benchmarks
