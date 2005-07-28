{-# OPTIONS -fglasgow-exts #-}

module Yi.Keymap.Killring where
import Yi.Editor
import Yi.Core
import Data.Dynamic

-- * Killring structure

data Killring = Killring {
                         krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: [String]
                         }
    deriving Typeable

instance Initializable Killring where
    initial = return $ Killring False False [[]]

-- * Killring "ADT"

killringMaxDepth :: Int
killringMaxDepth = 10

-- | Finish an atomic command, for the purpose of killring accumulation.
killringEndCmd :: Action
killringEndCmd = do Killring killed _ r <- getDynamic
                    setDynamic $ Killring False killed r

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
killringPut :: String -> Action
killringPut s = do Killring _ acc r@(x:xs) <- getDynamic 
                   if acc 
                      then setDynamic $ Killring True acc (s:take killringMaxDepth r)
                      else setDynamic $ Killring True acc ((x++s):xs)

-- | Return the killring contents as a list. Head is most recent.
killringGet :: IO [String]
killringGet = do Killring _ _ r <- getDynamic 
                 return r

-- | Construct a region from its bounds
mkRegion :: Int -> Int -> (Int, Int)
mkRegion x y = if x < y then (x,y) else (y,x)

-- * Killring actions

killRegionE :: Action
killRegionE = do m <- getMarkE
                 p <- getPointE
                 let r = mkRegion m p
                 text <- readRegionE r
                 killringPut text
                 deleteRegionE r
                 
yankE :: Action
yankE = do (text:_) <- killringGet 
           insertNE text
           
