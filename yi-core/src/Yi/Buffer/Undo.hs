{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}

-- | An implementation of restricted, linear undo, as described in:
--
-- >    T. Berlage, "A selective undo mechanism for graphical user interfaces
-- >    based on command objects", ACM Transactions on Computer-Human
-- >    Interaction 1(3), pp. 269-294, 1994.
--
-- Implementation based on a proposal by sjw.
--
-- From Berlage:
--
-- >    All buffer-mutating commands are stored (in abstract form) in an
-- >    Undo list. The most recent item in this list is the action that
-- >    will be undone next. When it is undone, it is removed from the Undo
-- >    list, and its inverse is added to the Redo list. The last command
-- >    put into the Redo list can be redone, and again prepended to the
-- >    Undo list. New commands are added to the Undo list without
-- >    affecting the Redo list.
--
-- Now, the above assumes that commands can be _redone_ in a state other
-- than that in which it was orginally done. This is not the case in our
-- text editor: a user may delete, for example, between an undo and a
-- redo. Berlage addresses this in S2.3. A Yi example:
--
-- >    Delete some characters
-- >    Undo partialy
-- >    Move prior in the file, and delete another _chunk_
-- >    Redo some things  == corruption.
--
-- Berlage describes the /stable execution property/:
--
-- >    A command is always redone in the same state that it was originally
-- >    executed in, and is always undone in the state that was reached
-- >    after the original execution.
--
-- >    The only case where the linear undo model violates the stable
-- >    execution property is when _a new command is submitted while the
-- >    redo list is not empty_. The _restricted linear undo model_ ...
-- >    clears the redo list in this case.
--
-- Also some discussion of this in: /The Text Editor Sam/, Rob Pike, pg 19.
--

module Yi.Buffer.Undo (
    emptyU
  , addChangeU
  , setSavedFilePointU
  , isAtSavedFilePointU
  , undoU
  , redoU
  , URList             {- abstractly -}
  , Change(AtomicChange, InteractivePoint)
   ) where

import           Data.Binary              (Binary (..))
import           Data.Monoid
import qualified Data.Sequence as S
import           GHC.Generics             (Generic)
import           Yi.Buffer.Implementation

data Change = SavedFilePoint
            | InteractivePoint
            | AtomicChange !Update
-- !!! It's very important that the updates are forced, otherwise
-- !!! we'll keep a full copy of the buffer state for each update
-- !!! (thunk) put in the URList.
            deriving (Show, Generic)
instance Binary Change

-- | A URList consists of an undo and a redo list.
data URList = URList !(S.Seq Change) !(S.Seq Change)
            deriving (Show, Generic)
instance Binary URList

-- | A new empty 'URList'.
-- Notice we must have a saved file point as this is when we assume we are
-- opening the file so it is currently the same as the one on disk
emptyU :: URList
emptyU = URList (S.singleton SavedFilePoint) S.empty

-- | Add an action to the undo list.
-- According to the restricted, linear undo model, if we add a command
-- whilst the redo list is not empty, we will lose our redoable changes.
addChangeU :: Change -> URList -> URList
addChangeU InteractivePoint (URList us rs) = URList (addIP us) rs
addChangeU u (URList us _) = URList (u S.<| us) S.empty

-- | Add a saved file point so that we can tell that the buffer has not
-- been modified since the previous saved file point.
-- Notice that we must be sure to remove the previous saved file points
-- since they are now worthless.
setSavedFilePointU :: URList -> URList
setSavedFilePointU (URList undos redos) =
  URList (SavedFilePoint S.<| cleanUndos) cleanRedos
  where
  cleanUndos = S.filter isNotSavedFilePoint undos

  cleanRedos = S.filter isNotSavedFilePoint redos

  isNotSavedFilePoint :: Change -> Bool
  isNotSavedFilePoint SavedFilePoint = False
  isNotSavedFilePoint _              = True

-- | This undoes one interaction step.
undoU :: Mark -> URList -> BufferImpl syntax -> (BufferImpl syntax, (URList, S.Seq Update))
undoU m = undoUntilInteractive m mempty . undoInteractive

-- | This redoes one iteraction step.
redoU :: Mark -> URList -> BufferImpl syntax -> (BufferImpl syntax, (URList, S.Seq Update))
redoU = asRedo . undoU

-- | Prepare undo by moving one interaction point from undoes to redoes.
undoInteractive :: URList -> URList
undoInteractive (URList us rs) = URList (remIP us) (addIP rs)

-- | Remove an initial interactive point, if there is one
remIP :: S.Seq Change -> S.Seq Change
remIP xs = case S.viewl xs of
  InteractivePoint S.:< xs' -> xs'
  _ -> xs

-- | Insert an initial interactive point, if there is none
addIP :: S.Seq Change -> S.Seq Change
addIP xs = case S.viewl xs of
  InteractivePoint S.:< _ -> xs
  _ -> InteractivePoint S.<| xs

-- | Repeatedly undo actions, storing away the inverse operations in the
--   redo list.
undoUntilInteractive :: Mark -> S.Seq Update -> URList -> BufferImpl syntax
                     -> (BufferImpl syntax, (URList, S.Seq Update))
undoUntilInteractive pointMark xs ur@(URList cs rs) b = case S.viewl cs of
  S.EmptyL -> (b, (ur, xs))
  SavedFilePoint S.:< (S.viewl -> S.EmptyL) -> (b, (ur, xs)) -- Why this special case?
  InteractivePoint S.:< _ -> (b, (ur, xs))
  SavedFilePoint S.:< cs' ->
    undoUntilInteractive pointMark xs (URList cs' (SavedFilePoint S.<| rs)) b
  AtomicChange u S.:< cs' ->
    let ur' = URList cs' (AtomicChange (reverseUpdateI u) S.<| rs)
        b' = applyUpdateWithMoveI u b
        (b'', (ur'', xs')) = undoUntilInteractive pointMark xs ur' b'
    in (b'', (ur'', u S.<| xs'))
    where
      -- Apply a /valid/ update and also move point in buffer to update position
      applyUpdateWithMoveI :: Update -> BufferImpl syntax -> BufferImpl syntax
      applyUpdateWithMoveI u = case updateDirection u of
                                 Forward ->  apply . move
                                 Backward -> move . apply
          where move = modifyMarkBI pointMark (\v -> v {markPoint = updatePoint u})
                apply = applyUpdateI u

-- | Run the undo-function @f@ on a swapped URList making it
--   operate in a redo fashion instead of undo.
asRedo :: (URList -> t -> (t, (URList, S.Seq Update))) -> URList -> t
       -> (t, (URList, S.Seq Update))
asRedo f ur x = let (y,(ur',rs)) = f (swapUndoRedo ur) x
                in (y,(swapUndoRedo ur',rs))
  where
    swapUndoRedo :: URList -> URList
    swapUndoRedo (URList us rs) = URList rs us

-- | undoIsAtSavedFilePoint. @True@ if the undo list is at a SavedFilePoint indicating
--   that the buffer has not been modified since we last saved the file.
-- Note: that an empty undo list does NOT mean that the buffer is not modified since
-- the last save. Because we may have saved the file and then undone actions done before
-- the save.
isAtSavedFilePointU :: URList -> Bool
isAtSavedFilePointU (URList us _) = isUnchanged us
  where
    isUnchanged cs = case S.viewl cs of
      S.EmptyL                  -> False
      SavedFilePoint S.:< _     -> True
      InteractivePoint S.:< cs' -> isUnchanged cs'
      _                         -> False
