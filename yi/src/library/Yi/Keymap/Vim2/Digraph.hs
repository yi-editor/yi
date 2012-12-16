module Yi.Keymap.Vim2.Digraph
    ( charFromDigraph
    ) where

import Prelude ()
import Yi.Prelude

import Control.Applicative
import Data.List (lookup)

charFromDigraph :: Char -> Char -> Maybe Char
charFromDigraph c1 c2 = lookup [c1, c2] digraphTable <|> lookup [c2, c1] digraphTable

digraphTable :: [(String, Char)]
digraphTable =
    [ ("ae", 'æ')
    , ("a'", 'á')
    , ("e'", 'é')
    , ("e`", 'è')
    , ("e^", 'ê')
    ]
