{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

-- | exports from "Yi.Config.Simple" which are useful to \"core yi\" rather than just config files.
module Yi.Config.Simple.Types
 where

import Control.Monad.State hiding (modify, get)
import Control.Monad.Base
import Control.Applicative
import Control.Lens
import Yi.Config
import Yi.Types

-- | The configuration monad. Run it with 'configMain'.
newtype ConfigM a = ConfigM {
    runConfigM :: StateT Config IO a
  } deriving (Monad, Functor, Applicative, MonadState Config, MonadBase IO)

-- | Fields that can be modified with all lens machinery.
type Field a = Lens' Config a

{- | Accessor for any 'YiConfigVariable', to be used by modules defining
'YiConfigVariable's. Such modules should provide a custom-named field.
For instance, take the following hypothetical 'YiConfigVariable':

@newtype UserName = UserName { unUserName :: String }
  deriving(Typeable, Binary, Default)
instance YiConfigVariable UserName

$(nameDeriveAccessors ''UserName (\n -> Just (n ++ \"A\")))

userName :: 'Field' 'String'
userName = unUserNameA '.' 'customVariable'@

Here, the hypothetical library would provide the field @userName@ to be used in preference to @customVariable@.
-}
customVariable :: YiConfigVariable a => Field a
customVariable = configVariable
