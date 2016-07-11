-- -*- haskell -*-
-- Maintainer: Andrew Myers
{
{-# OPTIONS -w  #-}
module Yi.Lexer.GitCommit  ( lexer, Token(..) ) where
import Data.Monoid (mappend)
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style ( StyleName )
import qualified Yi.Style as Style
}

$commitChars = [$printable\t] # [\#]
@diffStart = diff\ \-\-git\ $commitChars*
$nl        = [\n\r]
$notColon  = $printable # [:]

gitCommit :-

-- The first line of a git commit message is the digest that is
-- displayed as a summary of the commit in virtually all git tools.
<0> {
.+                             { c Style.regexStyle }
$nl                            { m (const SecondLine) Style.defaultStyle }
}

-- There should never be anything on the second line of a git commit message
-- so it is styled in a deliberately hideous color scheme.
<secondLine> {
.+                             { c (const $ Style.withFg Style.red `mappend` Style.withBg Style.brown) }
$nl                            { m (const MessageLine) Style.defaultStyle }
}

-- The body of a commit message is broken up as follows
-- * User's message
-- * git generated information in comments
-- * optional diff if commit was run with the -v option.
<body> {
^@diffStart$                   { m (const $ DiffDeclaration) Style.regexStyle }
\#                             { m (const $ LineComment) Style.commentStyle }
$commitChars*$                 { c Style.defaultStyle }
$white                         { c Style.defaultStyle }
.                              { c Style.defaultStyle }
}

-- Inside git generated comments specific information about what this
-- commit will do is displayed.  Highlight keywords and filenames.
-- The notColon rule highlights filenames not preceded by keywords.
-- The specific keywords rules switch to <keyword> context to highlight
-- everything to the end of the line (which should only ever be a filename.)
<lineComment> {
$nl                            { m (const MessageLine) Style.defaultStyle }
\t$notColon+$                  { c Style.preprocessorStyle }
"modified:"                    { m (const Keyword) Style.keywordStyle }
"new file:"                    { m (const Keyword) Style.keywordStyle }
"deleted:"                     { m (const Keyword) Style.keywordStyle }
.                              { c Style.commentStyle }
}

<keyword> {
$nl                            { m (const MessageLine) Style.defaultStyle }
.                              { c Style.preprocessorStyle }
}

-- Highlight diff lines
<diff> {
^@diffStart$                   { c Style.regexStyle }
^\@\@.*                        { c Style.keywordStyle }
^\- .*$                        { c Style.commentStyle }
^\+ .*$                        { c Style.operatorStyle }
^.*$                           { c Style.defaultStyle }
$white                         { c Style.defaultStyle }
.                              { c Style.defaultStyle }
}

{

data HlState = Digest
             | SecondLine
             | Keyword
             | MessageLine
             | LineComment
             | DiffDeclaration
          deriving (Show, Eq)

stateToInit Digest = 0
stateToInit SecondLine = secondLine
stateToInit Keyword = keyword
stateToInit MessageLine = body
stateToInit DiffDeclaration = diff
stateToInit LineComment = lineComment

initState :: HlState
initState = Digest

type Token = StyleName

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
