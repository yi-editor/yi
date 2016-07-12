module Yi.Lexer.Helpers.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

literally :: String -> Q Exp
literally = return . LitE . StringL

withRead :: String -> Q Exp
withRead s = return $ AppE (VarE $ mkName "read") (LitE $ StringL s)

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally
                  , quotePat = \s -> error s $ "quotePat: " ++ s
                  , quoteType = \s -> error $ "quoteType: " ++ s
                  , quoteDec = \s -> error $ "quoteDec: " ++ s}

rlit :: QuasiQuoter
rlit = QuasiQuoter { quoteExp = withRead
                   , quotePat = \s -> error s $ "quotePat: " ++ s
                   , quoteType = \s -> error $ "quoteType: " ++ s
                   , quoteDec = \s -> error $ "quoteDec: " ++ s}


-- | Read file as-is.
litFile :: QuasiQuoter
litFile = quoteFile lit

ritFile :: QuasiQuoter
ritFile = quoteFile rlit
