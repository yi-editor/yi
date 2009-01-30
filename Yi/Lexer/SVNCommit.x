-- -*- haskell -*-
{
{-# OPTIONS -w  #-}
module Yi.Lexer.SVNCommit
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
data HlState = CommitMessage
    deriving (Show)

stateToInit CommitMessage = 0

initState :: HlState
initState = CommitMessage

type Token = StyleName
#include "alex.hsinc"
}

