-- -*- haskell -*-
-- Maintainer: Corey O'Connor
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

@changeType = [^$white]+$white*

svnCommitMessage :-

<0>
{
    ^"--".*"--"$
        { m (const $ HlCommitSummary) Style.commentStyle }
    $white
        { c Style.defaultStyle }
    .
        { c Style.defaultStyle }
}

<commitSummary>
{
    ^@changeType
        { c Style.keywordStyle }
    $white
        { c Style.commentStyle }
    .
        { c Style.commentStyle }
}

{
data HlState = 
    HlCommitMessage
    | HlCommitSummary
    deriving (Show)

stateToInit HlCommitMessage = 0
stateToInit HlCommitSummary = commitSummary

initState :: HlState
initState = HlCommitMessage

type Token = StyleName
#include "alex.hsinc"
}

