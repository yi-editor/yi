module Yi.Keymap.Vim.TextObject
  ( TextObject(..)
  , CountedTextObject(..)
  , regionOfTextObjectB
  , changeTextObjectCount
  , changeTextObjectStyle
  , stringToTextObject
  ) where

import Control.Monad              (replicateM_, (<=<))
import Yi.Buffer.Adjusted
import Yi.Keymap.Vim.StyledRegion (StyledRegion (..), normalizeRegion)

data TextObject = TextObject !RegionStyle !TextUnit
data CountedTextObject = CountedTextObject !Int !TextObject

changeTextObjectCount :: Int -> CountedTextObject -> CountedTextObject
changeTextObjectCount n (CountedTextObject _ to) = CountedTextObject n to

regionOfTextObjectB :: CountedTextObject -> BufferM StyledRegion
regionOfTextObjectB = normalizeRegion <=< textObjectRegionB'

textObjectRegionB' :: CountedTextObject -> BufferM StyledRegion
textObjectRegionB' (CountedTextObject count (TextObject style unit)) =
    fmap (StyledRegion style) $ regionWithTwoMovesB
        (maybeMoveB unit Backward)
        (replicateM_ count $ moveB unit Forward)

changeTextObjectStyle :: (RegionStyle -> RegionStyle) -> TextObject -> TextObject
changeTextObjectStyle smod (TextObject s u) = TextObject (smod s) u

stringToTextObject :: String -> Maybe TextObject
stringToTextObject ('i':s) = parseTextObject InsideBound s
stringToTextObject ('a':s) = parseTextObject OutsideBound s
stringToTextObject _ = Nothing

parseTextObject :: BoundarySide -> String -> Maybe TextObject
parseTextObject bs (c:[]) = fmap (TextObject Exclusive . ($ bs == OutsideBound)) mkUnit
    where mkUnit = lookup c
           [('w',  toOuter unitViWord unitViWordAnyBnd)
           ,('W',  toOuter unitViWORD unitViWORDAnyBnd)
           ,('p',  toOuter unitEmacsParagraph unitEmacsParagraph) -- TODO inner could be inproved
           ,('s',  toOuter unitSentence unitSentence) -- TODO inner could be inproved
           ,('"',  unitDelimited '"' '"')
           ,('`',  unitDelimited '`' '`')
           ,('\'', unitDelimited '\'' '\'')
           ,('(',  unitDelimited '(' ')')
           ,(')',  unitDelimited '(' ')')
           ,('b',  unitDelimited '(' ')')
           ,('[',  unitDelimited '[' ']')
           ,(']',  unitDelimited '[' ']')
           ,('{',  unitDelimited '{' '}')
           ,('}',  unitDelimited '{' '}')
           ,('B',  unitDelimited '{' '}')
           ,('<',  unitDelimited '<' '>')
           ,('>',  unitDelimited '<' '>')
           -- TODO: 't'
           ]
parseTextObject _ _ = Nothing

-- TODO: this probably belongs to Buffer.TextUnit
toOuter :: TextUnit -> TextUnit -> Bool -> TextUnit
toOuter outer _     True  = leftBoundaryUnit outer
toOuter _     inner False = inner
