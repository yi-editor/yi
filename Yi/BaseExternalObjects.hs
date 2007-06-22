{- All the object files that must be loaded into the system before 
 - the dynamic main is started.
 -}
module Yi.BaseExternalObjects where

allExternalObjectFiles :: [String]
allExternalObjectFiles = ["YiUtils.o"]

