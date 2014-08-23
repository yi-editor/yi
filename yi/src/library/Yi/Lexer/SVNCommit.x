-- -*- haskell -*-
-- Maintainer: Corey O'Connor
{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.SVNCommit ( lexer ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
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

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
