import Prelude ()
import Yi
import Yi.Keymap.Vim
import qualified Yi.Keymap.Vim2 as V2
import qualified Yi.Keymap.Vim2.Common as V2
import qualified Yi.Keymap.Vim2.Utils as V2

import qualified Yi.Mode.Haskell as Haskell

main :: IO ()
main = yi $ defaultVimConfig {
    modeTable = (myModes ++ modeTable defaultVimConfig),
    defaultKm = myKeymap,
    configCheckExternalChangesObsessively = False
}

defaultSearchKeymap :: Keymap
defaultSearchKeymap = do
    Event (KASCII c) [] <- anyEvent
    write (isearchAddE [c])

myKeymap :: KeymapSet
myKeymap = V2.mkKeymapSet $ V2.defModeMapProto `override` \super self -> super {
        -- Here we can add custom bindings to different modes.
        -- See Yi.Keymap.Vim2.Common for datatypes and 
        -- Yi.Keymap.Vim2.Utils for useful functions like mkStringBindingE

        -- In case of conflict, that is if there exist multiple bindings
        -- whose prereq function returns WholeMatch,
        -- the first such binding is used.
        -- So it's important to have custom bindings first.
        V2.normalMap = myNormalBindings self ++ V2.normalMap super
    }

myNormalBindings :: V2.ModeMap -> [V2.VimBinding]
myNormalBindings modemap = fmap (V2.mkStringBindingE V2.Normal V2.Drop)
    [
    -- Tab traversal
      ("<C-h>", previousTabE, id)
    , ("<C-l>", nextTabE, id)

    -- Press space to clear incremental search highlight
    , (" ", resetRegexE, id)

    , ("<F3>", withBuffer0 deleteTrailingSpaceB, id)
    ]

myModes = [
         AnyMode Haskell.fastMode {
             -- Disable beautification
             modePrettify = const $ return ()
           , modeGetAnnotations = (const . const) []
         }
    ]