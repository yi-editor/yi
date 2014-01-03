module Yi.Config.Users.Anders (config) where

import Yi hiding (Block, (.))
import qualified Yi.Mode.Haskell as H
import Yi.Hoogle (hoogle)
import Yi.String (mapLines)
import Yi.Modes (removeAnnots)

config :: Config
config = defaultEmacsConfig

-- | Increase the indentation of the selection
increaseIndent :: BufferM ()
increaseIndent = do
  r <- getSelectRegionB 
  r' <- unitWiseRegion Yi.Line r 
     -- extend the region to full lines
  modifyRegionB (mapLines (' ':)) r'
     -- prepend each line with a space

main :: IO ()
main = yi $ config
  { defaultKm = defaultKm config
  , startFrontEnd = startFrontEnd config
  , modeTable =
    -- My precise mode with my hooks added
    AnyMode (haskellModeHooks H.preciseMode)
    -- My no annotations mode with mode hooks
    : AnyMode (haskellModeHooks noAnnots)
    : modeTable defaultConfig
  }

-- | Set my hooks for nice features
haskellModeHooks mode =
   mode { modeName = "my " ++ modeName mode
        , modeKeymap =
            topKeymapA %~ ((ctrlCh 'c' ?>> 
                            choice [ ctrlCh 'l' ?>>! H.ghciLoadBuffer
                                   , ctrl (char 'z') ?>>! H.ghciGet
                                   , ctrl (char 'h') ?>>! hoogle
                                   , ctrlCh 'r' ?>>! H.ghciSend ":r"
                                   , ctrlCh 't' ?>>! H.ghciInferType
                                   , ctrlCh 'n' ?>>! increaseIndent
                                   ])
                           <||) }

-- This is used in order to remove the unicode characters usually used.
noAnnots = removeAnnots (H.preciseMode {modeName = "preciseNoUnicode"})
