module Yi.Vty 
    (
     Pic,    
     
     module Graphics.Vty
    ) where

import Graphics.Vty

import qualified Yi.Event as Ev

type Pic = [[(Char,Attr)]]

