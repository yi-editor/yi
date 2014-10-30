import Graphics.Vty
import System.IO
import IncrementalParser2
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import Control.Monad

import Debug.Trace

main = do vt <- mkVty
          (sx,sy) <- getSize vt
          play vt (insert' 0 "" initial) 0 sx

play :: Vty -> Result Char a w r -> Int -> Int -> IO ()
play vt s p sx =
    do update vt (renderImg s p sx)
       k <- getEvent vt
       --trace (show k) (return())
       case k of 
         EvResize nx ny                 -> play vt s p nx
         EvKey KEsc   []                -> shutdown vt >> return ()
         _ -> let (s',p') = case k of 
                              EvKey KLeft  []         -> (s, p-1)
                              EvKey KRight []         -> (s, p+1)
                              EvKey KBS    []          
                                  | p > 0     -> (delete' (p-1) 1 s, p-1)
                                  | otherwise -> (s, p)
                              ev                      -> (insert' p (showBtl ev) s, p+1)
                              --_ -> (s, p)
              in play vt (repair' s') (max 0 $ min (getLength s') $ p') sx
{-
play :: Vty -> Result Char a w r -> Int -> Int -> IO ()
play vt s p sx = 
    do update vt (renderImg s p sx)
       k <- getEvent vt
       trace (show k) (return())
       case k of 
         EvResize nx ny                 -> play vt s p nx
         EvKey KEsc   []                -> shutdown vt >> return ()
         _ -> let (s',p') = case k of 
                              EvKey KLeft  []         -> (s, p-1)
                              EvKey KRight []         -> (s, p+1)
                              EvKey KBS    []         -> (delete' (p-1) 1 s, p-1)
                              ev                      -> (insert' p (showBtl ev) s, p+1)
              in play vt (repair' s') (max 0 $ min (getLength s') $ p') sx
-}

showBtl (EvKey (KASCII c) _)  = [c]
showBtl _ = ""

renderImg s p sx = pic { pCursor = Cursor cur 0,
                         pImage = img}
    where 
      (Just cur, img) = render (p, s) <||> 
                   (Just (p-getLength s), renderChar attr '|')

-- type Renderer :: Int -> (Maybe Int, Image)
-- (<+>) :: Renderer -> Renderer -> Renderer
-- r <+> s = \cur -> let (cur1, img1) = 

render :: (Int, Result Char a w r) -> (Maybe Int, Image)
render (cursor, r) = --trace (show (cursor,r)) $
    case r of 
      (Tip _ _) -> --trace "tip" $ 
          (Nothing, renderBS (setBG red attr) (B.pack ""))
      (Una [] (Just err) _ p) -> --trace "err" $
          (Nothing, renderBS (setBG red attr) (B.pack ".")) <||> render (cursor,p)
      (Una s err _ p) -> --trace "una" $
             (toMaybe (cursor < length s) cursor,
          renderBS (if isJust err then setFG red attr else attr) (B.pack s))
          <||> render (cursor - length s, p)
      (Bin n _a _p _err l r) -> --trace "bin" $
          render (cursor, l) <||> render (cursor - getLength l, r)

(ca,ia) <||> (cb,ib) = (ca `mplus` fmap (+ imgWidth ia) cb, ia <|> ib)

toMaybe False _ = Nothing
toMaybe True x = Just x
