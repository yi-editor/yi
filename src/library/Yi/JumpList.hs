{-# LANGUAGE TemplateHaskell, CPP, StandaloneDeriving, DeriveGeneric #-}

module Yi.JumpList
    ( JumpList
    , Jump(..)
    , addJump
    , jumpBack
    , jumpForward
    ) where

import Yi.Buffer.Basic

import Data.Binary
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif

import Data.List.PointedList as PL

type JumpList = Maybe (PL.PointedList Jump)

data Jump = Jump {
        jumpMark :: Mark
      , jumpBufferRef :: BufferRef
    }

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Jump)
#else
deriving instance Generic Jump
instance Binary Jump
#endif

instance Show Jump where
    show (Jump mark bufref) = "<Jump " ++ show mark ++ " " ++ show bufref ++ ">"

addJump :: Jump -> JumpList -> JumpList
addJump j (Just (PL.PointedList past present _future)) = Just $ PL.PointedList (present:past) j []
addJump j Nothing = Just $ PL.PointedList [] j []

jumpBack :: JumpList -> JumpList
jumpBack = modifyJumpList previous

jumpForward :: JumpList -> JumpList
jumpForward = modifyJumpList next

modifyJumpList :: (PointedList Jump -> Maybe (PointedList Jump)) -> JumpList -> JumpList
modifyJumpList f (Just jumps) = case f jumps of
                                Nothing -> Just jumps
                                Just jumps' -> Just jumps'
modifyJumpList _ Nothing = Nothing
