#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Data.Monoid
import Test.Tasty.TH
import Test.Tasty.HUnit

import qualified Yi.Rope as R

import Yi.Snippet.Internal

main :: IO ()
main = $defaultMainGenerator

lp :: Snippet
lp = Snippet "lp" $ do
  lit "{-# LANGUAGE "
  _ <- place "OverloadedStrings"
  line " #-}"

useSnippet :: Snippet -> [EditAction] -> (Int, R.YiString)
useSnippet snip actions =
    renderSnippet snip (foldl advanceEditState (initialEditState snip) actions)

case_lp_default :: Assertion
case_lp_default =
    useSnippet lp [] @?= (13, "{-# LANGUAGE OverloadedStrings #-}\n")

case_lp_default_escape :: Assertion
case_lp_default_escape =
    useSnippet lp [SEEscape] @?= (13, "{-# LANGUAGE OverloadedStrings #-}\n")

case_lp_default_complete :: Assertion
case_lp_default_complete =
    useSnippet lp [SENext] @?= (35, "{-# LANGUAGE OverloadedStrings #-}\n")

case_lp_lambdacase_incomplete :: Assertion
case_lp_lambdacase_incomplete =
    useSnippet lp (map SEInsertChar "LambdaCase")
        @?= (23, "{-# LANGUAGE LambdaCase #-}\n")

case_lp_lambdacase_complete :: Assertion
case_lp_lambdacase_complete =
    useSnippet lp (map SEInsertChar "LambdaCase" <> [SENext])
        @?= (28, "{-# LANGUAGE LambdaCase #-}\n")

case_lp_backspace :: Assertion
case_lp_backspace =
    useSnippet lp (map SEInsertChar "LambdaCasx" <> [SEBackSpace] <> [SEInsertChar 'e'])
        @?= (23, "{-# LANGUAGE LambdaCase #-}\n")

case_collect_vars :: Assertion
case_collect_vars =
    let Snippet _ body = lp
    in collectVars body @?= M.fromList [(UserVar 0, DefaultValue "OverloadedStrings")]
    
case_complete_edit_state :: Assertion
case_complete_edit_state =
    advanceEditState (initialEditState lp) SENext
    @?= EditState (Nothing, 0) (M.fromList [(UserVar 0, DefaultValue "OverloadedStrings")])