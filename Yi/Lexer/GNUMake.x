-- -*- haskell -*- 
{
{-# OPTIONS -w  #-}
module Yi.Lexer.GNUMake
  ( initState, alexScanToken ) 
where
import Yi.Lexer.Alex
import Yi.Style
  ( Style             ( .. )
  , StyleName
  )
import qualified Yi.Style as Style
}

make :-

<0>
{
    "#"[^\n]*
        { c Style.commentStyle }
    $white
        { c Style.defaultStyle }
    .
        { c Style.defaultStyle }
}

{
data HlState

stateToInit _ = 0

initState :: HlState
initState = undefined

type Token = StyleName
#include "alex.hsinc"
}

