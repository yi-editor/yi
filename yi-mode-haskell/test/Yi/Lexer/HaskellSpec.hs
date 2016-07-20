{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yi.Lexer.HaskellSpec (main, spec) where


import Control.Applicative ((<$>))
import Prelude hiding (lex)
import System.FilePath ((</>))
import Test.Hspec
import Yi.Buffer.Basic (Point(..))
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
import Yi.Lexer.Helpers.TH


deriving instance Read OpType
deriving instance Read ReservedType
deriving instance Read CommentType
deriving instance Read Token
deriving instance Read Posn
deriving instance Read Size
deriving instance Read Point
deriving instance Read a => Read (Tok a)

newState :: AlexState HlState
newState = AlexState initState 0 startPosn

mkIndx :: String -> IndexedStr
mkIndx = zip [1 ..]

newInput :: String -> AlexInput
newInput s = ('\n', [], mkIndx s)

lex :: String -> [(AlexState HlState, TT)]
lex s = unfoldLexer alexScanToken (newState, newInput s)

lexTok :: String -> [TT]
lexTok = map snd . lex

lexToks :: String -> [Token]
lexToks = map tokT . lexTok

shouldLexToS :: FilePath -> [Token] -> Expectation
shouldLexToS fp t = lexToks <$> readSample fp `shouldReturn` t

lexesToS :: String -> [Token] -> Spec
lexesToS s tt = it s $ s `shouldLexToS` tt

readSample :: FilePath -> IO String
readSample x = readFile $ "test" </> "test_data" </> x

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lexer tests" $ do
    it "fill me in" pending
    -- "Simple1.hs" `lexesToS` [ritFile|test/test_data/Simple1.hs_Token|]
    -- "Simple1Unicode.hs" `lexesToS` [ritFile|test/test_data/Simple1Unicode.hs_Token|]
    -- "UnicodeLiteral.hs" `lexesTo` [ritFile|test/test_data/UnicodeLiteral.hs_TToken|]
