{-# LANGUAGE Rank2Types, CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-} -- for good documentation, we want control over our export list, which occasionally gives us duplicate exports

{- | A simplified configuration interface for Yi. -}
module Yi.Config.Simple (
  -- $overview
  -- * The main interface
  ConfigM,
  configMain,
  Field,
  -- * Frontend
  setFrontendPreferences,
  setFrontend,
  -- * Modes, commands, and keybindings
  globalBindKeys,
  modeBindKeys,
  modeBindKeysByName,
  addMode,
  modifyMode,
  modifyModeByName,
  -- * Evaluation of commands
  evaluator,
  ghciEvaluator,
  publishedActionsEvaluator,
  publishAction,
  publishedActions,
  -- * Appearance
  fontName,
  fontSize,
  scrollWheelAmount,
  scrollStyle,
  ScrollStyle(..),
  cursorStyle,
  CursorStyle(..),
  Side(..),
  scrollBarSide,
  autoHideScrollBar,
  autoHideTabBar,
  lineWrap,
  windowFill,
  theme,
  -- ** Layout
  layoutManagers,
  -- * Debugging
  debug,
  -- * Startup hooks
  runOnStartup,
  runAfterStartup,
  -- * Advanced
  -- $advanced
  startActions,
  initialActions,
  defaultKm,
  inputPreprocess,
  modes,
  regionStyle,
  killringAccumulate,
  bufferUpdateHandler,
  -- * Module exports
  -- we can't just export 'module Yi', because then we would get clashes with Yi.Config
  module Yi.Boot,
--  module Yi.Buffer.Misc,
  module Yi.Core,
  module Yi.Dired,
  module Yi.File,
  module Yi.Config,
  module Yi.Config.Default,
  module Yi.Layout,
  module Yi.Search,
  module Yi.Style,
  module Yi.Style.Library,
  module Yi.Misc,
  module Yi.Mode.Haskell,
#ifdef SCION
  module Yi.Scion,
#endif
 ) where

import Yi.Boot
import Yi.Core hiding(modifyMode)
import Yi.Config.Default
import Yi.Config.Misc
import Yi.Config.Simple.Types
import Yi.Dired
import Yi.Eval
import Yi.File
import Yi.Layout
import Yi.Search
import Yi.Style
import Yi.Style.Library
import Yi.Misc
import Yi.Mode.Haskell
#ifdef SCION
import Yi.Scion
#endif
import Yi.Utils

import Text.Printf(printf)

import Control.Lens hiding (Action)
import Control.Applicative
import Control.Monad.State hiding (modify, get)

-- we do explicit imports because we reuse a lot of the names
import Yi.Config(Config, UIConfig,
                 startFrontEndA, configUIA, startActionsA, initialActionsA, defaultKmA,
                 configInputPreprocessA, modeTableA, debugModeA,
                 configRegionStyleA, configKillringAccumulateA, bufferUpdateHandlerA,
                 configVtyA, configFontNameA, configFontSizeA, configScrollWheelAmountA,
                 configScrollStyleA, configCursorStyleA, CursorStyle(..),
                 configLeftSideScrollBarA, configAutoHideScrollBarA, configAutoHideTabBarA,
                 configLineWrapA, configWindowFillA, configThemeA, layoutManagersA, configVarsA,
                )
import Data.Maybe(mapMaybe)

{- $overview
This module provides a simple configuration API, allowing users to start with an initial
configuration and imperatively (monadically) modify it. Some common actions (keybindings,
selecting modes, choosing the frontend) have been given special commands ('globalBindKeys',
'setFrontendPreferences', 'addMode', and so on).

A simple configuration might look like the following:

@import Yi.Config.Simple
import qualified Yi.Mode.Haskell as Haskell
-- note: don't import "Yi", or else there will be name clashes

main = 'configMain' 'defaultEmacsConfig' $ do
  'setFrontendPreferences' ["pango", "vty"]
  'fontSize' '%=' 'Just' 10
  'modeBindKeys' Haskell.cleverMode ('metaCh' \'q\' '?>>!' 'reload')
  'globalBindKeys' ('metaCh' \'r\' '?>>!' 'reload')@

A lot of the fields here are specified with the 'Field' type. To write a field, use ('%='). To read, use 'get'. For modification, use ('modify'). For example, the functions @foo@ and @bar@ are equivalent:

@foo = 'modify' 'layoutManagers' 'reverse'
bar = do
 lms <- 'get' 'layoutManagers'
 'layoutManagers' '%=' 'reverse' lms@
-}



--------------- Main interface
-- newtype ConfigM a   (imported)

-- | Starts with the given initial config, makes the described modifications, then starts yi.
configMain :: Config -> ConfigM () -> IO ()
configMain c m = yi =<< execStateT (runConfigM m) c

---------------------------------- Frontend
-- | Sets the frontend to the first frontend from the list which is installed.
--
-- Available frontends are a subset of: \"vty\", \"pango\", and \"batch\".
setFrontendPreferences :: [String] -> ConfigM ()
setFrontendPreferences fs =
   case mapMaybe (`lookup` availableFrontends) fs of
       (f:_) -> startFrontEndA .= f
       [] -> return ()

-- | Sets the frontend, if it is available.
setFrontend :: String -> ConfigM ()
setFrontend f = maybe (return ()) (startFrontEndA .=) (lookup f availableFrontends)

------------------------- Modes, commands, and keybindings
-- | Adds the given key bindings to the `global keymap'. The bindings will override existing bindings in the case of a clash.
globalBindKeys :: Keymap -> ConfigM ()
globalBindKeys a = (defaultKmA . topKeymapA) %= (||> a)

-- | @modeBindKeys mode keys@ adds the keybindings in @keys@ to all modes with the same name as @mode@.
--
-- As with 'modifyMode', a mode by the given name must already be registered, or the function will
-- have no effect, and issue a command-line warning.
modeBindKeys :: Mode syntax -> Keymap -> ConfigM ()
modeBindKeys mode keys = ensureModeRegistered "modeBindKeys" (modeName mode) $ modeBindKeysByName (modeName mode) keys

-- | @modeBindKeysByName name keys@ adds the keybindings in @keys@ to all modes with name @name@ (if it is registered). Consider using 'modeBindKeys' instead.
modeBindKeysByName :: String -> Keymap -> ConfigM ()
modeBindKeysByName name k = ensureModeRegistered "modeBindKeysByName" name $ modifyModeByName name (modeKeymapA %~ f)
 where
  f :: (KeymapSet -> KeymapSet) -> KeymapSet -> KeymapSet
  f mkm km = topKeymapA %~ (||> k) $ mkm km
-- (modeKeymapA %~ ((topKeymap %~ (||> k)) .))

-- | Register the given mode. It will be preferred over any modes already defined.
addMode :: Mode syntax -> ConfigM ()
addMode m = modeTableA %= (AnyMode m :)

-- | @modifyMode mode f@ modifies all modes with the same name as @mode@, using the function @f@.
--
-- Note that the @mode@ argument is only used by its 'modeName'. In particular, a mode by the given name
-- must already be registered, or this function will have no effect, and issue a command-line warning.
--
-- @'modifyMode' mode f = 'modifyModeByName' ('modeName' mode) f@
modifyMode :: Mode syntax -> (forall syntax'. Mode syntax' -> Mode syntax') -> ConfigM ()
modifyMode mode f = ensureModeRegistered "modifyMode" (modeName mode) $ modifyModeByName (modeName mode) f

-- | @modifyModeByName name f@ modifies the mode with name @name@ using the function @f@. Consider using 'modifyMode' instead.
modifyModeByName :: String -> (forall syntax. Mode syntax -> Mode syntax) -> ConfigM ()
modifyModeByName name f =
    ensureModeRegistered "modifyModeByName" name $ modeTableA %= fmap (onMode g)
        where
            g :: forall syntax. Mode syntax -> Mode syntax
            g m | modeName m == name = f m
                | otherwise          = m

-- helper functions
warn :: String -> String -> ConfigM ()
warn caller msg = io $ putStrLn $ printf "Warning: %s: %s" caller msg
-- the putStrLn shouldn't be necessary, but it doesn't print anything if it's not there...

isModeRegistered :: String -> ConfigM Bool
isModeRegistered name = any (\(AnyMode mode) -> modeName mode == name) <$> use modeTableA

-- ensure the given mode is registered, and if it is, then run the given action.
ensureModeRegistered :: String -> String -> ConfigM () -> ConfigM ()
ensureModeRegistered caller name m = do
  isRegistered <- isModeRegistered name
  if isRegistered
   then m
   else warn caller (printf "mode \"%s\" is not registered." name)

--------------------- Appearance
-- | 'Just' the font name, or 'Nothing' for default.
fontName :: Field (Maybe String)
fontName = configUIA . configFontNameA

-- | 'Just' the font size, or 'Nothing' for default.
fontSize :: Field (Maybe Int)
fontSize = configUIA . configFontSizeA

-- | Amount to move the buffer when using the scroll wheel.
scrollWheelAmount :: Field Int
scrollWheelAmount = configUIA . configScrollWheelAmountA

-- | 'Just' the scroll style, or 'Nothing' for default.
scrollStyle :: Field (Maybe ScrollStyle)
scrollStyle = configUIA . configScrollStyleA

-- | See 'CursorStyle' for documentation.
cursorStyle :: Field CursorStyle
cursorStyle = configUIA . configCursorStyleA

data Side = LeftSide | RightSide

-- | Which side to display the scroll bar on.
scrollBarSide :: Field Side
scrollBarSide = configUIA . configLeftSideScrollBarA . fromBool
  where
      fromBool :: Lens' Bool Side
      fromBool = lens (\b -> if b then LeftSide else RightSide) (\_ s -> case s of { LeftSide -> True; RightSide -> False })

-- | Should the scroll bar autohide?
autoHideScrollBar :: Field Bool
autoHideScrollBar = configUIA . configAutoHideScrollBarA

-- | Should the tab bar autohide?
autoHideTabBar :: Field Bool
autoHideTabBar = configUIA . configAutoHideTabBarA

-- | Should lines be wrapped?
lineWrap :: Field Bool
lineWrap = configUIA . configLineWrapA

-- | The character with which to fill empty window space. Usually \'~\' for vi-like editors, \' \' for everything else.
windowFill :: Field Char
windowFill = configUIA . configWindowFillA

-- | UI colour theme.
theme :: Field Theme
theme = configUIA . configThemeA

---------- Layout
-- | List of registered layout managers. When cycling through layouts, this list will be consulted.
layoutManagers :: Field [AnyLayoutManager]
layoutManagers = layoutManagersA

------------------------ Debugging
-- | Produce a .yi.dbg file with debugging information?
debug :: Field Bool
debug = debugModeA

----------- Startup hooks
-- | Run when the editor is started (this is run after all actions which have already been registered)
runOnStartup :: Action -> ConfigM ()
runOnStartup action = runManyOnStartup [action]

-- | List version of 'runOnStartup'.
runManyOnStartup :: [Action] -> ConfigM ()
runManyOnStartup actions = startActions %= (++ actions)

-- | Run after the startup actions have completed, or on reload (this is run after all actions which have already been registered)
runAfterStartup :: Action -> ConfigM ()
runAfterStartup action = runManyAfterStartup [action]

-- | List version of 'runAfterStartup'.
runManyAfterStartup :: [Action] -> ConfigM ()
runManyAfterStartup actions = initialActions %= (++ actions)

------------------------ Advanced
{- $advanced

These fields are here for completeness -- that is, to expose all the functionality
of the "Yi.Config" module. However, most users probably need not use these fields,
typically because they provide advanced functinality, or because a simpler interface
for the common case is available above.

-}

-- | Actions to run when the editor is started. Consider using 'runOnStartup' or 'runManyOnStartup' instead.
startActions :: Field [Action]
startActions = startActionsA

-- | Actions to run after startup or reload. Consider using 'runAfterStartup' or 'runManyAfterStartup' instead.
initialActions :: Field [Action]
initialActions = initialActionsA

-- | Default keymap to use.
defaultKm :: Field KeymapSet
defaultKm = defaultKmA

-- | ?
inputPreprocess :: Field (P Event Event)
inputPreprocess = configInputPreprocessA

-- | List of modes by order of preference. Consider using 'addMode', 'modeBindKeys', or 'modifyMode' instead.
modes :: Field [AnyMode]
modes = modeTableA

-- | Set to 'Exclusive' for an emacs-like behaviour. Consider starting with 'defaultEmacsConfig', 'defaultVimConfig', or 'defaultCuaConfig' to instead.
regionStyle :: Field RegionStyle
regionStyle = configRegionStyleA

-- | Set to 'True' for an emacs-like behaviour, where all deleted text is accumulated in a killring. Consider starting with 'defaultEmacsConfig', 'defaultVimConfig', or 'defaultCuaConfig' instead.
killringAccumulate :: Field Bool
killringAccumulate = configKillringAccumulateA

-- | ?
bufferUpdateHandler :: Field [[Update] -> BufferM ()]
bufferUpdateHandler = bufferUpdateHandlerA

