{-
  A simple haskell script to output a config.mk
  file for inclusion into a cabal-make.
  The advantage of this is that the user is not constantly
  bugged by the patches caused by changing the configuration
  which they will never wish to record since they are not
  appropriate for the general distribution.
-}

module Main
  ( main )
where

{- External Library Module Imports -}
{- Standard Library Module Imports -}
import System.Environment
  ( getArgs )
{- Local Module Imports -}
{- End of Module Imports -}

main :: IO ()
main = do args <- getArgs
          let contents = processArgs args
              outfile  = "config.test"
          if args == [ "--help" ]
             then putStrLn helpUsage
             else writeFile outfile contents

helpUsage :: String
helpUsage = unwords [ "runhaskell"
                    , " MkConfig.hs"
                    , "[ --prefix PREFIX ]"
                    , "[ --haddock-interface IFACE ]"
                    , "[ --user USERNAME ]"
                    , "[ --frontend {vty|gtk} ]"
                    ]

processArgs :: [ String ] -> String
processArgs args =
  unlines [ "# where to install executable and packages (running in-place -- without installing -- is not supported)"
          , "frontend = " ++ frontend
          , ""
          , "prefix = " ++ prefix 
          , ""
          , "# The following are web-publishing options."
          , ""
          , "tmp-dir = /tmp"
          , "user = " ++ user
          , ""
          , "configure-dirs = --prefix=$(prefix) "
          , "hscolour-css = $(cabal-make)/hscolour.css"
          , ""
          , "haddock-interfaces=\\"
          , haddock
          , ""
          , ""
          , "extra-configure-args = --user"
          , ""
          , "HsColour = HsColour"
          , ""
          , ""
          , ""
          , "# You should never need to change the following."
          , ""
          , "top-src-dir ="
          , ""
          ]
  where
  frontend = maybe "vty" id $ getOptionArg "--frontend" args
  prefix   = maybe "$(HOME)/install" id $ getOptionArg "--prefix" args
  haddock  = maybe haddockDefault id $ getOptionArg "--haddock-interfaces" args
  user     = maybe "" id $ getOptionArg "--user" args

  haddockDefault = 
    unlines [ "http://haskell.org/ghc/docs/latest/html/libraries/base \\"
            , "http://haskell.org/ghc/docs/latest/html/libraries/QuickCheck"
            ]

getNoArgOption :: String -> [ String ] -> Bool
getNoArgOption = elem 

getOptionArg :: String -> [ String ] -> Maybe String
getOptionArg s []     = Nothing
getOptionArg s [ _ ]  = Nothing  -- really an error if the last element = option
getOptionArg s (s1 : s2 : rest)
  | s == s1   = Just s2
  | otherwise = getOptionArg s rest