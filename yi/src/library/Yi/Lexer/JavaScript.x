-- -*- haskell -*-
--
-- Simple lexer for JavaScript 1.7
-- (C) Copyright 2009 Deniz Dogan
--
-- Note that this mode is for JavaScript 1.7, i.e. Mozilla, not ECMAScript, nor
-- JScript, but it does take much of its information from the ECMAScript
-- specification.
--
-- Things about JavaScript deserve to be mentioned:
-- * JavaScript explicitly forbids nested comments, so we don't have to care
--   about the "level" of nesting in multiline comments
-- * JavaScript supports making variables for which the name clashes with
--   constructors, e.g. Array, but you cannot use "new Array()" to create an
--   array if you do that.
--
-- Sources for reserved words:
-- https://developer.mozilla.org/En/Core_JavaScript_1.5_Reference/Reserved_Words
-- https://developer.mozilla.org/en/New_in_JavaScript_1.6
-- https://developer.mozilla.org/en/New_in_JavaScript_1.7
--
-- Sources for the general syntax:
-- http://en.wikipedia.org/wiki/JavaScript_syntax
-- http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}

module Yi.Lexer.JavaScript ( initState, alexScanToken, tokenToStyle,
                             TT, Token(..), Reserved(..), Operator(..),
                             HlState, prefixOperators, infixOperators,
                             postfixOperators ) where

import Data.Monoid (Endo(..))
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style

}

$whitechar  = [\ \t\n\r\f\v]

@builtin    = true | false | null | undefined
@reservedid = break | case | catch | continue | default | delete | do | else
            | finally | for | function | if | in | instanceof | new | return
            | switch | this | throw | try | typeof | var | void | while | with
            | @builtin

@ops = "+"   | "-"   | "*"   | "/"   | "%"   | "++"  | "--"  | "="   | "+="
     | "-="  | "*="  | "/="  | "%="  | "=="  | "!="  | ">"   | ">="  | "<"
     | "<="  | "===" | "!==" | "&&"  |"||"   | "!"   | "&"   | "|"   | "^"
     | "<<"  | ">>"  | ">>>" | "~"   | "."

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff]
$special   = [\(\)\,\;\[\]\{\}\:\?]

$digit     = 0-9
$hexit     = [0-9 A-F a-f]
@decimal   = $digit+
@hexal     = "0x" $hexit+

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\@\\\^\|\-\~\_]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \:\"\']
$graphic   = [$small $large $symbol $digit $special \:\"\']
$name      = [a-zA-Z0-9\_\$] -- Valid characters for a "name"

-- Octals are sort of "implicit" in JavaScript. "0307" is a valid octal, but
-- "0309" is the exact same thing as "309."  Since we don't make any distinction
-- between different types of numbers, we completely disregard octals here, they
-- Just Work (tm).
@number  = @decimal | @hexal
@gap     = \\ $whitechar
@string1 = $graphic # [\"\\] | " " | @gap
@string2 = $graphic # [\'\\] | " " | @gap

-- @varid represents valid names for variables and functions.
@varid   = [$name # [0-9A-Z]] $name*
@const   = [$name # [0-9a-z]] $name* -- Constants and constructors

haskell :-

<0> {

$whitechar+      ;
$special         { cs $ (Special . head) } -- All of the special symbols are characters

@number          { cs $ Number }
@ops             { cs $ (Op . opToOp) }
@reservedid      { cs $ (Res . resToRes) }
@varid           { cs $ ValidName }
@const           { cs $ Const }

\" @string1* \"  { cs $ Str }
\' @string2* \'  { cs $ Str }
\/ [^\/]+ \/     { cs $ Rex }
"//".*           { c  $ Comment Line }
"/*"             { m (subtract 1) $ Comment Start }
"<!--"           { m (+1) $ Comment Start }

.                { c  $ Unknown }

}

<multicomm> {
"*/"         { m (+1) $ Comment End }
$whitechar+  ;
[^\*]+       { c $ Comment Text }
.            { c $ Comment Text }
}

<htmlcomm> {
"-->"        { m (subtract 1) $ Comment End }
$whitechar+  ;
[^\-]        { c $ Comment Text }
.            { c $ Comment Text }
}


{

type TT = Tok Token

-- | The @Line@ constructor represents a single-line comment.  @Start@ and @End@
--   represent the start and end of a multi-line comment, respectively.  @Text@
--   represents the text in a multi-line comment.
data CommentType = Line | Start | End | Text
                 deriving (Show, Eq)

-- | The constructors for @Reserved@ have an apostrophe as a suffix because
--   @Default@ is already used.  Also note that @Undefined'@ is not intended as
--   some sort of "backup" reserved word for things we don't care about -- it
--   really means the "undefined" built-in in JavaScript.
data Reserved = Break' | Case' | Catch' | Continue' | Default' | Delete' | Do'
              | Else' | Finally' | For' | Function' | If' | In' | InstanceOf'
              | New' | Return' | Switch' | This' | Throw' | Try' | TypeOf'
              | Var' | Void' | While' | With' | True' | False' | Null'
              | Undefined'
                deriving (Show, Eq)

-- | The constructors for @Operator@ have an apostrophe as a suffix because
--   e.g. @LT@ is already used by @Prelude@.
data Operator = Add' | Subtract' | Multiply' | Divide' | Modulo' | Increment'
              | Decrement' | Assign' | AddAssign' | SubtractAssign'
              | MultiplyAssign' | DivideAssign' | ModuloAssign' | Equals'
              | NotEquals' | GT' | GTE' | LT' | LTE' | EqualsType'
              | NotEqualsType' | And' | Or' | Not' | BitAnd' | BitOr' | BitXor'
              | LeftShift' | RightShift' | RightShiftZ' | BitNot' | Qualify'
                deriving (Show, Eq)

-- | Prefix operators.  NOTE: Add' is also a valid prefix operator, but since
--   it's completely useless in the real world, we don't care about it here.
--   Doing this makes parsing much, much easier.
prefixOperators = [ Subtract', Increment', Decrement', BitNot', Not' ]

-- | Postfix operators.
postfixOperators = [ Increment', Decrement' ]

-- | Infix operators.
infixOperators = [ Add', Subtract', Multiply', Divide', Modulo', Assign',
                   AddAssign', SubtractAssign', MultiplyAssign', DivideAssign',
                   ModuloAssign', Equals', NotEquals', GT', GTE', LT', LTE',
                   EqualsType', NotEqualsType', And', Or', BitAnd', BitOr',
                   BitXor', LeftShift', RightShift', RightShiftZ', Qualify' ]

-- | @HlState@ is 0 when outside of a multi-line comment and -1 when inside one.
type HlState = Int

-- | The different tokens.
data Token = Unknown
           | Res !Reserved
           | Str !String
           | Rex !String
           | Op !Operator
           | Special !Char
           | Number !String
           | ValidName !String
           | Comment !CommentType
           | Const !String
             deriving (Show, Eq)

stateToInit x | x < 0 = multicomm
              | x > 0 = htmlcomm
              | otherwise = 0

initState :: HlState
initState = 0

-- | Takes a 'Token' and returns a style to be used for that type of token.
--
--   TODO: The `elem` check is potentially unnecessarily slow.  We could split
--   the Const constructor into two different ones, one for builtins and one for
--   others.
tokenToStyle (Comment Line) = commentStyle
tokenToStyle (Comment _)    = blockCommentStyle
tokenToStyle (Const x)      | x `elem` builtinConstructors = builtinStyle
                            | otherwise = typeStyle
tokenToStyle (Number _)     = numberStyle
tokenToStyle (Res _)        = keywordStyle
tokenToStyle (Str _)        = stringStyle
tokenToStyle (Rex _)        = regexStyle
tokenToStyle Unknown        = errorStyle
tokenToStyle _              = defaultStyle

builtinConstructors :: [String]
builtinConstructors = [ "Array", "String", "RegExp", "Function", "Date"
                      , "Boolean", "Object" ]

-- | Given a @String@ representing an operator, returns an 'Operator' with the
--   appropriate constructor.
opToOp :: String -> Operator
opToOp "+"   = Add'
opToOp "-"   = Subtract'
opToOp "*"   = Multiply'
opToOp "/"   = Divide'
opToOp "%"   = Modulo'
opToOp "++"  = Increment'
opToOp "--"  = Decrement'
opToOp "="   = Assign'
opToOp "+="  = AddAssign'
opToOp "-="  = SubtractAssign'
opToOp "*="  = MultiplyAssign'
opToOp "/="  = DivideAssign'
opToOp "%="  = ModuloAssign'
opToOp "=="  = Equals'
opToOp "!="  = NotEquals'
opToOp ">"   = GT'
opToOp ">="  = GTE'
opToOp "<"   = LT'
opToOp "<="  = LTE'
opToOp "===" = EqualsType'
opToOp "!==" = NotEqualsType'
opToOp "&&"  = And'
opToOp "||"  = Or'
opToOp "!"   = Not'
opToOp "&"   = BitAnd'
opToOp "|"   = BitOr'
opToOp "^"   = BitXor'
opToOp "<<"  = LeftShift'
opToOp ">>"  = RightShift'
opToOp ">>>" = RightShiftZ'
opToOp "~"   = BitNot'
opToOp "."   = Qualify'

-- | Given a @String@ representing a reserved word, returns a 'Reserved' with
--   the appropriate constructor.
resToRes :: String -> Reserved
resToRes "break"      = Break'
resToRes "case"       = Case'
resToRes "catch"      = Catch'
resToRes "continue"   = Continue'
resToRes "default"    = Default'
resToRes "delete"     = Delete'
resToRes "do"         = Do'
resToRes "else"       = Else'
resToRes "finally"    = Finally'
resToRes "for"        = For'
resToRes "function"   = Function'
resToRes "if"         = If'
resToRes "in"         = In'
resToRes "instanceof" = InstanceOf'
resToRes "new"        = New'
resToRes "return"     = Return'
resToRes "switch"     = Switch'
resToRes "this"       = This'
resToRes "throw"      = Throw'
resToRes "try"        = Try'
resToRes "typeof"     = TypeOf'
resToRes "var"        = Var'
resToRes "void"       = Void'
resToRes "while"      = While'
resToRes "with"       = With'
resToRes "true"       = True'
resToRes "false"      = False'
resToRes "null"       = Null'
resToRes "undefined"  = Undefined'

#include "common.hsinc"

}
