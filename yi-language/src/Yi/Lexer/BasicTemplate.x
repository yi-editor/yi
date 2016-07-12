-- -*- haskell -*-
{
{-# OPTIONS -w  #-}
module Yi.Lexer.BasicTemplate
  ( initState, alexScanToken )
where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
  ( Style             ( .. )
  , StyleName
  )
import qualified Yi.Style as Style
}

main :-

<0>
{
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
#include "common.hsinc"
}

