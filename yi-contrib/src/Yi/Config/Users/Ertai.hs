{-# LANGUAGE TypeFamilies #-}
module Yi.Config.Users.Ertai (config) where

import Yi
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Syntax.Haskell as Haskell
import qualified Yi.Lexer.Haskell as Haskell
import qualified Yi.Syntax.Strokes.Haskell as Haskell
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Foldable (Foldable)
import Yi.Char.Unicode (greek, symbols)
import Control.Monad (replicateM_)
import Control.Applicative
import Control.Lens
import Yi.Keymap.Keys (char,(?>>!),(>>!))
import Yi.Lexer.Alex (Tok)
import qualified Yi.Syntax.Tree as Tree
import Yi.Hoogle
import Yi.Buffer
import Yi.Keymap.Vim (viWrite, v_ex_cmds, v_top_level, v_ins_char, v_opts, tildeop, savingInsertStringB, savingDeleteCharB, exCmds, exHistInfixComplete')
import Yi.Keymap (withModeY)
import Yi.MiniBuffer (matchingBufferNames)
import qualified Yi.Keymap.Vim as Vim

myModetable :: [AnyMode]
myModetable = [
               AnyMode $ haskellModeHooks Haskell.cleverMode
              ,
               AnyMode $ haskellModeHooks Haskell.preciseMode
              ,
               AnyMode $ haskellModeHooks Haskell.fastMode
              ,
               AnyMode . haskellModeHooks $ Haskell.cleverMode
              ,
               AnyMode $ haskellModeHooks Haskell.fastMode
              ,
               AnyMode . haskellModeHooks $ Haskell.fastMode
              ]

type Endom a = a -> a

haskellModeHooks :: (Foldable f) => Endom (Mode (f Haskell.TT))
haskellModeHooks mode =
  -- uncomment for shim:
  -- Shim.minorMode $ 
     mode {
        -- modeAdjustBlock = \_ _ -> return (),
        -- modeGetStrokes = \_ _ _ _ -> [],
        modeName = "my " ++ modeName mode,
        -- example of Mode-local rebinding
        modeKeymap = topKeymapA %~
            ((char '\\' ?>> choice [char 'l' ?>>! Haskell.ghciLoadBuffer,
                                    char 'z' ?>>! Haskell.ghciGet,
                                    char 'h' ?>>! hoogle,
                                    char 'r' ?>>! Haskell.ghciSend ":r",
                                    char 't' ?>>! Haskell.ghciInferType
                                   ])
                      <||)
     }

{-
main :: IO ()
main = do args <- getArgs
          if any ("--as=" `isPrefixOf`) args
            then yi defaultConfig
            else yi $ myConfig defaultVimConfig
-}

config :: Config
config = defaultVimConfig { modeTable = fmap (onMode prefIndent) (myModetable ++ modeTable defaultVimConfig)
                          , defaultKm = Vim.mkKeymap extendedVimKeymap
                          , startActions = startActions defaultVimConfig ++ [makeAction (maxStatusHeightA .= 10 :: EditorM ())]
                          }

-- Set soft tabs of 4 spaces in width.
prefIndent :: Mode s -> Mode s
prefIndent m = m { modeIndentSettings = IndentSettings { expandTabs = True
                                                       , shiftWidth = 2
                                                       , tabSize = 2 }
                 }

mkInputMethod :: [(String,String)] -> Keymap
mkInputMethod xs = choice [pString i >> adjustPriority (negate (length i)) >>! savingInsertStringB o | (i,o) <- xs]

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
                    <|> ((events $ map char "\\u") >>! unicodifySymbols)
                    <|> ((events $ map char "\\c") >>! withModeY modeToggleCommentSelection)
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
    , v_opts = (v_opts super) { tildeop = True }
    , v_ex_cmds = exCmds [("b",
                       withEditor . switchToBufferWithNameE,
                       Just $ exHistInfixComplete' True matchingBufferNames)]
    }

