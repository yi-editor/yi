{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# OPTIONS -#include "YiUtils.h" #-}

-- Copyright (C) 2007, 2008 Stefan O'Rear

-- | This module defines the list of syntax highlighter modules.

module Yi.Syntax.Table
  ( ExtHL(..)
  , highlighters
  )
where

{- Standard Library Modules Imported -}
import qualified Data.Map as M
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Yi.Syntax
  ( Highlighter )
import qualified Yi.Syntax.Haskell
import qualified Yi.Syntax.LitterateHaskell (highlighter)
import qualified Yi.Syntax.Latex
import qualified Yi.Syntax.Srmc
import qualified Yi.Syntax.Cabal
import qualified Yi.Syntax.Cplusplus
{- End of Imports -}

data ExtHL = forall a. Eq a => ExtHL (Maybe (Highlighter a))

highlighters :: M.Map String ExtHL
highlighters = M.fromList highList


highList :: [ (String, ExtHL) ]
highList =
  [ ( "haskell"  , ExtHL (Just Yi.Syntax.Haskell.highlighter) )
  , ("lithaskell", ExtHL (Just Yi.Syntax.LitterateHaskell.highlighter) )
  , ( "latex"    , ExtHL (Just Yi.Syntax.Latex.highlighter) )
  , ( "cabal"    , ExtHL (Just Yi.Syntax.Cabal.highlighter) )
  , ( "cplusplus", ExtHL (Just Yi.Syntax.Cplusplus.highlighter) )
  , ( "srmc"     , ExtHL (Just Yi.Syntax.Srmc.highlighter) )
  , ( "none"     , ExtHL (Nothing :: Maybe (Highlighter ())) )
  ]
