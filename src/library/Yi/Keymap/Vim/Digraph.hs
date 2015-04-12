module Yi.Keymap.Vim.Digraph
    ( charFromDigraph
    , defDigraphs
    ) where

import Control.Applicative (Alternative ((<|>)))

charFromDigraph :: [(String, Char)] -> Char -> Char -> Maybe Char
charFromDigraph digraphTable c1 c2 =
    lookup [c1, c2] digraphTable <|> lookup [c2, c1] digraphTable

defDigraphs :: [(String, Char)]
defDigraphs =
    [ ("ae",  'æ')
    , ("a'",  'á')
    , ("e'",  'é')
    , ("e`",  'è')
    , ("o\"", 'ő')
    , ("o:",  'ö')
    , ("a:",  'ä')
    , ("e:",  'ë')
    , ("u:",  'ü')
    , ("AE",  'Æ')
    , ("Ae",  'Æ')
    , ("A'",  'Á')
    , ("E'",  'É')
    , ("E`",  'È')
    , ("O\"", 'Ő')
    , ("O:",  'Ö')
    , ("A:",  'Ä')
    , ("E:",  'Ë')
    , ("U:",  'Ü')
    , ("=e",  '€')
    , ("Cu",  '¤')
    , ("+-",  '±')
    , ("-+",  '∓')
    , ("^1",  '¹')
    , ("^2",  '²')
    , ("^3",  '³')
    , ("^4",  '⁴')
    , ("^5",  '⁵')
    , ("^6",  '⁶')
    , ("^7",  '⁷')
    , ("^8",  '⁸')
    , ("^9",  '⁹')
    , ("0S",  '⁰')
    , ("1S",  '¹')
    , ("2S",  '²')
    , ("3S",  '³')
    , ("4S",  '⁴')
    , ("5S",  '⁵')
    , ("6S",  '⁶')
    , ("7S",  '⁷')
    , ("8S",  '⁸')
    , ("9S",  '⁹')
    , ("0S",  '⁰')
    , ("0s",  '₀')
    , ("1s",  '₁')
    , ("2s",  '₂')
    , ("3s",  '₃')
    , ("4s",  '₄')
    , ("5s",  '₅')
    , ("6s",  '₆')
    , ("7s",  '₇')
    , ("8s",  '₈')
    , ("9s",  '₉')
    , ("0s",  '₀')
    , ("'0",  '˚')
    ]
