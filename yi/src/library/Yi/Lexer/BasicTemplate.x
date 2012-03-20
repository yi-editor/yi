-- -*- haskell -*-
{
{-# OPTIONS -w  #-}
{-# LANGUAGE BangPatterns  #-}
module Yi.Lexer.BasicTemplate
  ( initState, alexScanToken )
where
import Yi.Lexer.Alex
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

