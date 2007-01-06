module Yi.Vty 
    (
     Pic,    
     
     module Graphics.Vty
    ) where

import Graphics.Vty

type Pic = [[(Char,Attr)]]

