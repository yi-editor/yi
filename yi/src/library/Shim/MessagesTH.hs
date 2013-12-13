{-# LANGUAGE TemplateHaskell #-}
module Shim.MessagesTH where

import Data.Char
import Data.IORef
import Data.List
import Data.Monoid
import Language.Haskell.TH
import Shim.Sexp
import Shim.Utils
import Shim.SHM
import Debug.Trace
import System.IO.Unsafe

data Message argumentsType responseType =
  Message { name ::String
          , callback ::  argumentsType -> SHM (Response responseType) }

data Response a = Error String
                | Response a

instance Functor Response where
    fmap f (Response a) = Response (f a)
    fmap f (Error msg)  = Error msg

data PackedMsg where Pack :: (ConvSexp res, ConvSexp args) => Message args res -> PackedMsg


data NoArgs
newtype Singleton a = Singleton a

msgList :: IORef [Name]
msgList = unsafePerformIO $ newIORef []

mkMsg fun = do
    VarI _ ty _ _ <- reify fun
    let adapt = case arity ty of
                  0 -> noArgs
                  1 -> singleton
                  2 -> uncurry
                  3 -> uncurry3
                  4 -> uncurry4
                  n -> error ("mkMsg: add mode arities (" ++ show n ++ ")")
    body   <- [| Message msgName $(adapt fun) |]
--    msg_ty <- [t| ArgsToMsg ty args_ty => Message args_ty return_ty|]
--    type_dec <- [d| decName :: msg_ty |]
    runIO $ modifyIORef msgList (decName :)
    return [-- type_dec,
             FunD decName [Clause [] (NormalB body) []]]
 where
  msgName = camelCaseToLisp (nameBase fun)
  decName = mkName$ nameBase fun ++ "Msg"
  noArgs   name = [|\ () -> $(varE name) |]
  singleton name= [|\ (Singleton x) -> $(varE name) x |]
  uncurry  name = [|\(x, y) -> $(varE name) x y|]
  uncurry3 name = [|\(x,y,z) -> $(varE name) x y z|]
  uncurry4 name = [|\(x,y,z,w) -> $(varE name) x y z w|]

mkMessageList = do
  msgs <- runIO $ readIORef msgList
  let entries = map (\name -> [| Pack $(varE name) |]) msgs
  [d| messages = $(listE entries) |]

camelCaseToLisp :: String -> String
camelCaseToLisp = map toLower . concat . intersperse "-" . splitBy isUpper

-- arity ty | trace (show ty) False = undefined
arity (AppT ArrowT arg) = 1 + arity arg
arity (AppT fun arg) = arity fun + arity arg
arity _ = 0

