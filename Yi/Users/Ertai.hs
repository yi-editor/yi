import Yi
import qualified Yi.Mode.Haskell as Haskell
import Yi.Prelude
import Prelude (map)
import Data.List (isPrefixOf, reverse, length)
import Data.Maybe
import Yi.Char.Unicode (greek, symbols)
import Control.Monad (replicateM_)
import Yi.Keymap.Keys (char,(?>>!),(>>!))
import Yi.Misc (adjBlock)
import Yi.Buffer
import Yi.Keymap.Vim (viWrite, v_top_level, v_ins_char, savingInsertStringE, savingDeleteCharB)
import qualified Yi.Keymap.Vim as Vim

myModetable :: [AnyMode]
myModetable = [
               AnyMode Haskell.cleverMode
              ,
               AnyMode Haskell.fastMode
              ]

main :: IO ()
main = yi $ myConfig defaultVimConfig

myConfig :: Config -> Config
myConfig cfg = cfg { modeTable = fmap (onMode prefIndent) (myModetable ++ modeTable cfg)
                   , defaultKm = Vim.mkKeymap extendedVimKeymap
                   }

-- Set soft tabs of 4 spaces in width.
prefIndent :: Mode s -> Mode s
prefIndent m = m { modeIndentSettings = IndentSettings { expandTabs = True
                                                       , shiftWidth = 2
                                                       , tabSize = 2 }
                 }

mkInputMethod :: [(String,String)] -> Keymap
mkInputMethod xs = choice [pString i >> adjustPriority (negate (length i)) >>! savingInsertStringE o | (i,o) <- xs]

extraInput :: Keymap
extraInput = ctrl (char ']') ?>> mkInputMethod (greek ++ symbols)

-- need something better
unicodifySymbols :: BufferM ()
unicodifySymbols = modifyRegionB f =<< regionOfB unitViWORD
  where f x = fromMaybe x $ lookup x (greek ++ symbols)

extendedVimKeymap :: Proto Vim.ModeMap
extendedVimKeymap = Vim.defKeymap `override` \super self -> super
    { v_top_level = (deprioritize >> v_top_level super)
                    <|> (char ',' ?>>! viWrite)
                    <|> ((events $ map char "|u") >>! unicodifySymbols)
                    <|> ((events $ map char "\\c") >>! withModeB modeToggleCommentSelection)
    , v_ins_char =
            (deprioritize >> v_ins_char super)
            -- I want softtabs to be deleted as if they are tabs. So if the
            -- current col is a multiple of 4 and the previous 4 characters
            -- are spaces then delete all 4 characters.
            <|> (spec KBS ?>>! do
                    c <- curCol
                    line <- readRegionB =<< regionOfPartB Line Backward
                    sw <- indentSettingsB >>= return . shiftWidth
                    let indentStr = replicate sw ' '
                        toDel | (c `mod` sw) == 0 && indentStr `isPrefixOf` reverse line = sw
                              | otherwise                                                = 1
                    replicateM_ toDel $ savingDeleteCharB Backward
                )
            <|> (adjustPriority (-1) >> extraInput)
    }

