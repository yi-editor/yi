{-# LANGUAGE TemplateHaskell #-}

module Yi.JumpList
    ( JumpList(..)
    , Jump(..)
    , jumpListSingleton
    , addJump
    , jumpBack
    , jumpForward
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Buffer.Basic
import System.FilePath

import Data.Binary
import Data.DeriveTH

import Data.List.PointedList as PL

data JumpList = JumpList (PL.PointedList Jump)

data Jump = Jump {
        jumpPoint :: Point
      , jumpBufferRef :: BufferRef
    }

$(derive makeBinary ''JumpList)
$(derive makeBinary ''Jump)

jumpListSingleton :: BufferRef -> JumpList
jumpListSingleton b = JumpList $ PL.singleton $ Jump 0 b

instance Show Jump where
    show (Jump point bufref) = "<Jump " ++ show point ++ " " ++ show bufref ++ ">"

addJump :: Jump -> JumpList -> JumpList
addJump j (JumpList (PL.PointedList past focus _future)) =
     JumpList $ PL.PointedList (focus:past) j []

jumpBack :: JumpList -> JumpList
jumpBack = modifyJumpList previous

jumpForward :: JumpList -> JumpList
jumpForward = modifyJumpList next

modifyJumpList :: (PointedList Jump -> Maybe (PointedList Jump)) -> JumpList -> JumpList
modifyJumpList f (JumpList jumps) = case f jumps of
                                Nothing -> JumpList jumps
                                Just jumps' -> JumpList jumps'
