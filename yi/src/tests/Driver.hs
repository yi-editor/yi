{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving #-}

module Driver where

import System.Environment
import Control.Monad
import Test.QuickCheck hiding (promote)
import System.Random hiding (next)
import Text.Printf
import Data.List            (sort,group,intersperse)

-- Following code shamelessly stolen from XMonad.
main :: (Read t, Num t, PrintfArg t1, Num b, PrintfArg b) =>
                                    [(t1, t -> IO (Bool, b))] -> IO ()
main tests  = do
    args <- fmap (drop 1) getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- fmap unzip $ mapM (\(s,a) -> printf "%-25s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed) :: IO ()
    when (not . and $ results) $ fail "Not all tests passed!"

------------------------------------------------------------------------
--
-- QC driver
--


debug :: Bool
debug = False

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck (stdArgs {maxSuccess = n}) a
-- mytest a n = mycheck stdArgs
--     { maxSuccess=n 
--    , configEvery   = \o _ -> let s = show o in s ++ [ '\b' | _ <- s ] } a
 -- , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Args -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    results <- quickCheckWithResult config {replay = Just (rnd, 1)} a
    print results
    return $ case results of
        Success {} ->(True, maxSuccess config)
        GaveUp {numTests = n} ->(True, n)
        Failure {} -> (False, 0)
        NoExpectedFailure {} -> (True, 0)

-- mytests :: Args -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
-- mytests config gen rnd0 ntest nfail stamps
--     | ntest == maxTest config = done "OK," ntest stamps >> return (True, ntest)
--     | nfail == maxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
--     | otherwise               =
--       do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
--          case ok result of
--            Nothing    ->
--              mytests config gen rnd1 ntest (nfail+1) stamps
--            Just True  ->
--              mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
--            Just False ->
--              putStr ( "Falsifiable after "
--                    ++ show ntest
--                    ++ " tests:\n"
--                    ++ unlines (arguments result)
--                     ) >> hFlush stdout >> return (False, ntest)
--      where
--       result      = generate (maxSize config ntest) rnd2 gen
--       (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
            . map entry
            . reverse
            . sort
            . map pairLength
            . group
            . sort
            . filter (not . null)
            $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    pairLength []         = (0, [])
    entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,h) -> (fromIntegral x, h)

