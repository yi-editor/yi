import Graphics.Vty
import System.IO
import IP
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)

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
                              EvKey KLeft  [] | p > 0 -> (s, p-1)
                              EvKey KRight []         -> (s, p+1)
                              EvKey KBS    []         -> (delete' (p-1) 1 s, p-1)
                              ev                      -> (insert' p (showBtl ev) s, p+1)
              in play vt (repair s') p' sx

showBtl (EvKey (KASCII c) _)  = [c]
showBtl _ = ""

renderImg (s,left) p sx = pic { pCursor = Cursor p 0,
                                pImage = render s
                                         <|> renderBS (setFG yellow attr) (B.pack left) <|> renderChar attr '|'  }
render :: (Result Char a w r) -> Image
render r = 
    case r of 
      (Tip _ _) -> empty
      (Una [] (Just err) _ p) -> renderBS (setBG red attr) (B.pack " ") <|> render p
      (Una s err _ p) -> renderBS (if isJust err then setFG red attr else attr) (B.pack s) <|> render p
      (Bin n _a _p _err l r) -> render l <|> render r


