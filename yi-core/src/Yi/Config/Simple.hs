{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config.Simple
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A simplified configuration interface for Yi.
--
-- This module provides a simple configuration API, allowing users to
-- start with an initial configuration and imperatively (monadically)
-- modify it. Some common actions (keybindings, selecting modes,
-- choosing the frontend) have been given special commands
-- ('globalBindKeys', 'setFrontendPreferences', 'addMode', and so on).
--
-- A simple configuration might look like the following:
--
-- @
-- import Yi.Config.Simple
-- import Yi.Boot
-- import qualified Yi.Mode.Haskell as Haskell
-- -- note: don't import "Yi", or else there will be name clashes
--
-- main = 'configMain' 'defaultEmacsConfig' $ do
--   'fontSize' '%=' 'Just' 10
--   'modeBindKeys' Haskell.cleverMode ('metaCh' \'q\' '?>>!' 'reload')
--   'globalBindKeys' ('metaCh' \'r\' '?>>!' 'reload')
-- @
--
-- A lot of the fields here are specified with the 'Field' type. To write
-- a field, use ('%='). To read, use 'get'. For modification, use
-- ('modify'). For example, the functions @foo@ and @bar@ are equivalent:
--
-- @
-- foo = 'modify' 'layoutManagers' 'reverse'
-- bar = do
--  lms <- 'get' 'layoutManagers'
--  'layoutManagers' '%=' 'reverse' lms
-- @

module Yi.Config.Simple (
  -- * The main interface
  ConfigM,
  Field,
  -- * Modes, commands, and keybindings
  globalBindKeys,
  modeBindKeys,
  modeBindKeysByName,
  addMode,
  modifyMode,
  modifyModeByName,
  -- * Evaluation of commands
  evaluator,
#ifdef HINT
  ghciEvaluator,
#endif
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

  -- we can't just export 'module Yi', because then we would get
  -- clashes with Yi.Config
  module Yi.Buffer,
  module Yi.Core,
  module Yi.Dired,
  module Yi.Editor,
  module Yi.File,
  module Yi.Config,
  module Yi.Config.Default,
  module Yi.Keymap,
  module Yi.Keymap.Keys,
  module Yi.Layout,
  module Yi.Search,
  module Yi.Style,
  module Yi.Style.Library,
  module Yi.Misc,
 ) where

import           Lens.Micro.Platform (Lens', (%=), (%~), use, lens)
import qualified Data.Text as T
import qualified Data.Sequence as S
import           Text.Printf(printf)
import           Yi.Buffer hiding (modifyMode)
import           Yi.Config.Default
import           Yi.Config.Misc
import           Yi.Config.Simple.Types
import           Yi.Core
import           Yi.Dired
import           Yi.Editor
import           Yi.Eval
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Layout
import           Yi.Misc
import           Yi.Search
import           Yi.Style
import           Yi.Style.Library
import           Yi.Utils

-- we do explicit imports because we reuse a lot of the names
import           Yi.Config(Config, UIConfig, startFrontEndA, configUIA,
                           startActionsA, initialActionsA, defaultKmA,
                           configInputPreprocessA, modeTableA, debugModeA,
                           configRegionStyleA, configKillringAccumulateA,
                           bufferUpdateHandlerA, configFontNameA,
                           configFontSizeA, configScrollWheelAmountA,
                           configScrollStyleA, configCursorStyleA,
                           CursorStyle(..), configLeftSideScrollBarA,
                           configAutoHideScrollBarA, configAutoHideTabBarA,
                           configLineWrapA, configWindowFillA, configThemeA,
                           layoutManagersA, configVarsA,
                           )


--------------- Main interface
-- newtype ConfigM a   (imported)

------------------------- Modes, commands, and keybindings
-- | Adds the given key bindings to the `global keymap'. The bindings
-- will override existing bindings in the case of a clash.
globalBindKeys :: Keymap -> ConfigM ()
globalBindKeys a = (defaultKmA . topKeymapA) %= (||> a)

-- | @modeBindKeys mode keys@ adds the keybindings in @keys@ to all
-- modes with the same name as @mode@.
--
-- As with 'modifyMode', a mode by the given name must already be
-- registered, or the function will have no effect, and issue a
-- command-line warning.
modeBindKeys :: Mode -> Keymap -> ConfigM ()
modeBindKeys mode keys =
  ensureModeRegistered "modeBindKeys" (modeName mode) boundKeys
  where
    boundKeys = modeBindKeysByName (modeName mode) keys

-- | @modeBindKeysByName name keys@ adds the keybindings in @keys@ to
-- all modes with name @name@ (if it is registered). Consider using
-- 'modeBindKeys' instead.
modeBindKeysByName :: T.Text -> Keymap -> ConfigM ()
modeBindKeysByName name k =
  ensureModeRegistered "modeBindKeysByName" name modMode
  where
    f :: (KeymapSet -> KeymapSet) -> KeymapSet -> KeymapSet
    f mkm km = topKeymapA %~ (||> k) $ mkm km

    modMode = modifyModeByName name (modeKeymapA %~ f)

-- | Register the given mode. It will be preferred over any modes
-- already defined.
addMode :: Mode -> ConfigM ()
addMode m = modeTableA %= (m :)

-- | @modifyMode mode f@ modifies all modes with the same name as
-- @mode@, using the function @f@.
--
-- Note that the @mode@ argument is only used by its 'modeName'. In
-- particular, a mode by the given name must already be registered, or
-- this function will have no effect, and issue a command-line
-- warning.
--
-- @'modifyMode' mode f = 'modifyModeByName' ('modeName' mode) f@
modifyMode :: Mode
           -> (Mode -> Mode)
           -> ConfigM ()
modifyMode mode f = ensureModeRegistered "modifyMode" (modeName mode) modMode
  where
    modMode = modifyModeByName (modeName mode) f

-- | @modifyModeByName name f@ modifies the mode with name @name@
-- using the function @f@. Consider using 'modifyMode' instead.
modifyModeByName :: T.Text
                 -> (Mode -> Mode)
                 -> ConfigM ()
modifyModeByName name f =
    ensureModeRegistered "modifyModeByName" name $ modeTableA %= fmap g
        where
            g :: Mode -> Mode
            g m | modeName m == name = f m
                | otherwise          = m

-- helper functions
warn :: String -> String -> ConfigM ()
warn caller msg = io $ putStrLn $ printf "Warning: %s: %s" caller msg
-- the putStrLn shouldn't be necessary, but it doesn't print anything
-- if it's not there...

isModeRegistered :: T.Text -> ConfigM Bool
isModeRegistered name =
  any (\mode -> modeName mode == name) <$> use modeTableA

-- ensure the given mode is registered, and if it is, then run the given action.
ensureModeRegistered :: String -> T.Text -> ConfigM () -> ConfigM ()
ensureModeRegistered caller name m = do
  isRegistered <- isModeRegistered name
  if isRegistered
   then m
   else warn caller (printf "mode \"%s\" is not registered." (T.unpack name))

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
    fromBool = lens (\b -> if b then LeftSide else RightSide)
                    (\_ s -> case s of { LeftSide -> True; RightSide -> False })

-- | Should the scroll bar autohide?
autoHideScrollBar :: Field Bool
autoHideScrollBar = configUIA . configAutoHideScrollBarA

-- | Should the tab bar autohide?
autoHideTabBar :: Field Bool
autoHideTabBar = configUIA . configAutoHideTabBarA

-- | Should lines be wrapped?
lineWrap :: Field Bool
lineWrap = configUIA . configLineWrapA

-- | The character with which to fill empty window space. Usually
-- \'~\' for vi-like editors, \' \' for everything else.
windowFill :: Field Char
windowFill = configUIA . configWindowFillA

-- | UI colour theme.
theme :: Field Theme
theme = configUIA . configThemeA

---------- Layout
-- | List of registered layout managers. When cycling through layouts,
-- this list will be consulted.
layoutManagers :: Field [AnyLayoutManager]
layoutManagers = layoutManagersA

------------------------ Debugging
-- | Produce a .yi.dbg file with debugging information?
debug :: Field Bool
debug = debugModeA

----------- Startup hooks
-- | Run when the editor is started (this is run after all actions
-- which have already been registered)
runOnStartup :: Action -> ConfigM ()
runOnStartup action = runManyOnStartup [action]

-- | List version of 'runOnStartup'.
runManyOnStartup :: [Action] -> ConfigM ()
runManyOnStartup actions = startActions %= (++ actions)

-- | Run after the startup actions have completed, or on reload (this
-- is run after all actions which have already been registered)
runAfterStartup :: Action -> ConfigM ()
runAfterStartup action = runManyAfterStartup [action]

-- | List version of 'runAfterStartup'.
runManyAfterStartup :: [Action] -> ConfigM ()
runManyAfterStartup actions = initialActions %= (++ actions)

------------------------ Advanced
{- $advanced

These fields are here for completeness -- that is, to expose all the
functionality of the "Yi.Config" module. However, most users probably
need not use these fields, typically because they provide advanced
functinality, or because a simpler interface for the common case is
available above.

-}

-- | Actions to run when the editor is started. Consider using
-- 'runOnStartup' or 'runManyOnStartup' instead.
startActions :: Field [Action]
startActions = startActionsA

-- | Actions to run after startup or reload. Consider using
-- 'runAfterStartup' or 'runManyAfterStartup' instead.
initialActions :: Field [Action]
initialActions = initialActionsA

-- | Default keymap to use.
defaultKm :: Field KeymapSet
defaultKm = defaultKmA

-- | ?
inputPreprocess :: Field (P Event Event)
inputPreprocess = configInputPreprocessA

-- | List of modes by order of preference. Consider using 'addMode',
-- 'modeBindKeys', or 'modifyMode' instead.
modes :: Field [Mode]
modes = modeTableA

-- | Set to 'Exclusive' for an emacs-like behaviour. Consider starting
-- with 'defaultEmacsConfig', 'defaultVimConfig', or
-- 'defaultCuaConfig' to instead.
regionStyle :: Field RegionStyle
regionStyle = configRegionStyleA

-- | Set to 'True' for an emacs-like behaviour, where all deleted text
-- is accumulated in a killring. Consider starting with
-- 'defaultEmacsConfig', 'defaultVimConfig', or 'defaultCuaConfig'
-- instead.
killringAccumulate :: Field Bool
killringAccumulate = configKillringAccumulateA

-- | ?
bufferUpdateHandler :: Field (S.Seq (S.Seq Update -> BufferM ()))
bufferUpdateHandler = bufferUpdateHandlerA
