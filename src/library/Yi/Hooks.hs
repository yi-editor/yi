{- | This module provides assistance in implementing \"hooks\" in Yi.
This module provides no major new functionality -- only assistance in
using 'YiConfigVariable's more easily to implement hooks.

We consider a simple example. Suppose we have a function

> promptForFile :: Maybe FilePath -> YiM FilePath

which prompts the user to select a file from their file system,
starting with the provided directory (if actually provided). Since
this is a frequent task in Yi, it is important for it to be as
user-friendly as possible. If opinions vary on the meaning of
\"user-friendly\", then we would really like to provide multiple
implementations of @promptForFile@, and allow users to select which
implementation to use in their config files.

A way to achieve this is using hooks, as follows:

> -- create a new type
> newtype FilePrompter = FilePrompter
>   { runFilePrompter :: Maybe FilePath -> YiM FilePath }
>   deriving (Typeable)
> $(nameDeriveAccessors ''FilePrompter (n -> Just (n ++ "A")))
>
> -- give some implementations
> filePrompter1, filePrompter2, filePrompter3 :: FilePrompter
> ...
>
> -- declare FilePrompter as a YiConfigVariable (so it can go in the Config)
> instance YiConfigVariable FilePrompter
>
> -- specify the default FilePrompter
> instance Default FilePrompter where
>    def = filePrompter1
>
> -- replace the old promptForFile function with a shim
> promptForFile :: Maybe FilePath -> YiM FilePath
> promptForFile = runHook runFilePrompter
>
> -- provide a custom-named Field for Yi.Config.Simple (not
> -- strictly necessary, but user-friendly)
> filePrompter :: Field FilePrompter
> filePrompter = customVariable

The user can write

> ...
>    filePrompter %= filePrompter2
> ...

in their config file, and calls to @promptForFile@ will now use the
different prompter. Library code which called @promptForFile@ does not
need to be changed, but it gets the new @filePrompter2@ behaviour
automatically.

See "Yi.Eval" for a real example of hooks.
-}

module Yi.Hooks(
  -- * Convenience function 'runHook'
  runHook,
  HookType,
  -- * Re-exports from "Yi.Config.Simple"
  customVariable,
  Field,
 )
  where

import Control.Lens           ((^.))
import Yi.Config              (configVariable)
import Yi.Config.Simple.Types (Field, customVariable)
import Yi.Editor              (EditorM, askCfg)
import Yi.Keymap              (YiM)
import Yi.Types               (YiConfigVariable)

-- | Looks up the configured value for the hook, and runs it. The
-- argument to 'runHook' will typically be a record accessor. See
-- 'HookType' for the valid hook types.
runHook :: (HookType ty, YiConfigVariable var) => (var -> ty) -> ty
runHook = runHookImpl

-- | The class of \"valid hooks\". This class is exported abstractly,
-- but the instances can be phrased quite simply: the functions (of
-- arbitrarily many arguments, including zero) which run in either the
-- 'EditorM' or 'YiM' monads.
--
--A typical example would be something like
--
-- @Int -> String -> 'EditorM' String@.
class HookType ty where
    runHookImpl :: YiConfigVariable var => (var -> ty) -> ty

instance HookType (EditorM a) where
    runHookImpl lookupHook = do
        cfg <- askCfg
        lookupHook (cfg ^. configVariable)
instance HookType (YiM a) where
    runHookImpl lookupHook = do
        cfg <- askCfg
        lookupHook (cfg ^. configVariable)
instance HookType b => HookType (a -> b) where
    runHookImpl lookupHook a = runHookImpl (($a) . lookupHook)
