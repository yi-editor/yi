{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Option
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Command-line options

module Yi.Option
    (
    -- * Types
      YiOption
    , YiOptionDescr
    , YiOptions
    , OptionError(..)

    -- * Core
    , yiCustomOptions
    , consYiOption
    , consYiOptions

    -- * Argument-less options
    , yiBoolOption
    , yiFlagOption
    , yiActionFlagOption

    -- * Argument-taking options
    , yiStringOption
    , yiStringOption'
    , yiActionOption
    , yiActionOption'
    )
where

import           Data.Default          (Default)
import qualified Data.Text             as T (Text)
import           Data.Typeable         (Typeable)
import           Data.String           (IsString, fromString)
import           Lens.Micro.Platform   (Lens', makeLenses, over, set)
import           System.Exit           (ExitCode)
import           System.Console.GetOpt (OptDescr, ArgDescr(..))
import           Yi.Config.Lens        (configVariable, startActionsA)
import           Yi.Types              (Action, Config, YiConfigVariable)

data OptionError = OptionError T.Text ExitCode

-- | An option is a function that attempts to change the configuration of the
-- editor at runtime.
type YiOption = Config -> Either OptionError Config

type YiOptionDescr = OptDescr YiOption

-- | Custom options that should be accepted. Provided in user configuration.
--
-- The general flow is that the user adds options to his configuration. Options
-- are essentially functions describing how to modify the configuration at runtime.
-- When an option is called, it gets the current config and may modify it (to encode
-- its value)
newtype YiOptions = YiOptions { _yiOptions :: [YiOptionDescr] }
    deriving (Default, Typeable)

instance YiConfigVariable YiOptions

makeLenses ''YiOptions

-- | Lens for accessing the list of custom options.
--
-- You can pretty much create whatever types of options you want with this.
-- But most cases are taken care of by one of the helper functions in this module.
yiCustomOptions :: Lens' Config [YiOptionDescr]
yiCustomOptions = configVariable . yiOptions

-- | Includes an extra option in the configuration. Small wrapper around 'yiCustomOptions'
consYiOption :: YiOptionDescr -> Config -> Config
consYiOption opt = over yiCustomOptions (opt:)

-- | Like 'consYiOption' but supports multiple options. Convenient for keymaps which might
-- want to install lots of options.
consYiOptions :: [YiOptionDescr] -> Config -> Config
consYiOptions opts = over yiCustomOptions (opts++)

-- | An argument which sets some configuration value to 'True'.
yiBoolOption :: Lens' Config Bool -> ArgDescr YiOption
yiBoolOption lens = NoArg $ Right . set lens True

-- | An argument which applies a function transforming some inner value of
-- the configuration.
yiFlagOption :: Lens' Config a -> (a -> a) -> ArgDescr YiOption
yiFlagOption lens f = NoArg $ Right . over lens f

-- | Flag that appends an action to the startup actions.
yiActionFlagOption :: Action -> ArgDescr YiOption
yiActionFlagOption action = NoArg f
    where f config = Right $ over startActionsA (++[action]) config
 
-- | Sets the value of an option which is any string type (hopefully text...)
--
-- This is not meant to be fully applied. By only passing in the lens you
-- will obtain a value suitable for use in OptDescr.
yiStringOption :: IsString a => Lens' Config a -> String -> ArgDescr YiOption
yiStringOption lens desc = ReqArg f desc
    where f string config = Right $ set lens (fromString string) config

-- | Just like 'yiStringOption', except it applies a 'Just'. Useful for setting
-- string-like values whose default is 'None'.
yiStringOption' :: IsString a => Lens' Config (Maybe a) -> String -> ArgDescr YiOption
yiStringOption' lens desc = ReqArg f desc
    where f string config = Right $ set lens (Just $ fromString string) config

-- | Option that appends a parameterized action to the startup actions.
yiActionOption :: IsString a => (a -> Action) -> String -> ArgDescr YiOption
yiActionOption action desc = ReqArg f desc
    where f string config = Right $ over startActionsA (++[action (fromString string)]) config

yiActionOption' :: IsString a => (a -> Either OptionError Action) -> String -> ArgDescr YiOption
yiActionOption' action desc = ReqArg f desc
    where f string config = do
            action' <- action (fromString string)
            return $ over startActionsA (++[action']) config
