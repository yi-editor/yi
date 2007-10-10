import Graphics.Vty
import System.IO
import IncrementalParser2
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import Control.Monad

main = do vt <- mkVty
          (sx,sy) <- getSize vt
          play vt (insert' 0 "a*b+c+d" initial) 0 sx

play vt s p sx = 
    do update vt (renderImg s p sx)
       k <- getEvent vt
       case k of 
         EvResize nx ny                 -> play vt s p nx
         EvKey KEsc   []                -> shutdown vt >> return ()
         _ -> let (s',p') = case k of 
                              EvKey KLeft  []         -> (s, p-1)
                              EvKey KRight []         -> (s, p+1)
                              EvKey KBS    []         -> (delete' (p-1) 1 s, p-1)
                              ev                      -> (insert' p (showBtl ev) s, p+1)
              in play vt (repair s') (max 0 $ min (getLength (fst s')) $ p') sx

showBtl (EvKey (KASCII c) _)  = [c]
showBtl _ = ""

renderImg (s,left) p sx = pic { pCursor = Cursor cur 0,
                                pImage = img <|> renderChar attr '|'}
    where 
      (Just cur, img) = render (p, s) <||> 
                   (Just (p-getLength s), 
                    renderBS (setFG yellow attr) (B.pack left))

-- type Renderer :: Int -> (Maybe Int, Image)
-- (<+>) :: Renderer -> Renderer -> Renderer
-- r <+> s = \cur -> let (cur1, img1) = 

render :: (Int, Result Char a w r) -> (Maybe Int, Image)
render (cursor, r) = 
    case r of 
      (Tip _ _) -> (Nothing, empty)
      (Una [] (Just err) _ p) -> 
          (Nothing, renderBS (setBG red attr) (B.pack ".")) <||> render (cursor,p)
      (Una s err _ p) -> 
          (toMaybe (cursor < length s) cursor,
          renderBS (if isJust err then setFG red attr else attr) (B.pack s))
          <||> render (cursor - length s, p)
      (Bin n _a _p _err l r) -> render (cursor, l) <||> render (cursor - getLength l, r)

(ca,ia) <||> (cb,ib) = (ca `mplus` fmap (+ imgWidth ia) cb, ia <|> ib)

toMaybe False _ = Nothing
toMaybe True x = Just x
