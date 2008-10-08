{- An example yi.hs that uses the Vim keymap with these additions:
    - Always uses the VTY UI by default.
    - The color style is darkBlueTheme
    - The insert mode of the Vim keymap has been extended with a few additions
      I find useful.
 -}
import Yi.Prelude
import Prelude ()

import Yi
import Yi.Keymap.Vim
import Yi.Buffer.Indent (indentAsPreviousB)
import Yi.Keymap.Keys

import qualified Yi.UI.Vty

import Yi.Style.Library (darkBlueTheme)
import Data.List (isPrefixOf, reverse)
import Control.Monad (replicateM_)

-- Set soft tabs of 4 spaces in width.
prefIndent :: Mode s -> Mode s
prefIndent m = m {
        modeIndentSettings = IndentSettings
            {
                expandTabs = True,
                shiftWidth = 4,
                tabSize = 4
            }}


main :: IO ()
main = yi $ defaultConfig 
    {
        -- Use VTY as the default UI.
        startFrontEnd = Yi.UI.Vty.start,
        defaultKm = mkKeymap extendedVimKeymap,
        modeTable = fmap (onMode prefIndent) (modeTable defaultConfig),
        configUI = (configUI defaultConfig)
            {
                configTheme = darkBlueTheme
            }
    }

extendedVimKeymap = defKeymap `override` \super self -> super
    {
        v_top_level = 
            (deprioritize >> v_top_level super)
            -- On 'o' in normal mode I always want to use the indent of the previous line.
            -- TODO: If the line where the newline is to be inserted is inside a
            -- block comment then the block comment should be "continued"
            <|> (char 'o' ?>> beginIns self $ do 
                    moveToEol
                    insertB '\n'
                    indentAsPreviousB
                    return ()
                ),
        v_ins_char = 
            (deprioritize >> v_ins_char super) 
            -- On enter I always want to use the indent of previous line
            -- TODO: If the line where the newline is to be inserted is inside a
            -- block comment then the block comment should be "continued"
            <|> ( spec KEnter ?>>! do
                    insertB '\n'
                    indentAsPreviousB
                    return ()
                )
            -- I want softtabs to be deleted as if they are tabs. So if the 
            -- current col is a multiple of 4 and the previous 4 characters
            -- are spaces then delete all 4 characters.
            -- TODO: Incorporate into Yi itself.
            <|> ( spec KBS ?>>! do
                    c <- curCol
                    line <- readRegionB =<< regionOfPartB Line Backward
                    let i = if (c `mod` 4) /= 0
                                then 1
                                else if "    " `isPrefixOf` reverse line 
                                    then 4
                                    else 1
                    replicateM_ i $ deleteB Character Backward
                )
            -- On starting to write a block comment I want the close comment 
            -- text inserted automatically.
            <|> choice 
                [ pString open_tag >>! do
                    insertN $ open_tag ++ " \n" 
                    indentAsPreviousB
                    insertN $ " " ++ close_tag
                    lineUp
                 | (open_tag, close_tag) <- 
                    [ ("{-", "-}") -- Haskell block comments
                    , ("/*", "*/") -- C++ block comments
                    ]
                ]
    }


