{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Yi.JumpList
    ( JumpList
    , Jump(..)
    , addJump
    , jumpBack
    , jumpForward
    ) where

import           GHC.Generics          (Generic)

import           Data.Binary           (Binary)
import           Data.List.PointedList as PL (PointedList (..), next, previous)
import           Yi.Buffer.Basic       (BufferRef, Mark)


type JumpList = Maybe (PL.PointedList Jump)

data Jump = Jump {
        jumpMark      :: Mark
      , jumpBufferRef :: BufferRef
    } deriving (Generic)

instance Binary Jump

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
