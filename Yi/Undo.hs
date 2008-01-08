--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
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

module Yi.Undo (
    emptyUR
  , addUR
  , setSavedPointUR
  , manyUR
  , undoInteractivePoint
  , undoUR
  , redoUR
  , isUnchangedUList
  , reverseUpdate
  , getActionB
  , URList             {- abstractly -}
  , Change(AtomicChange, InteractivePoint)
  , changeUpdates
   ) where

import Yi.FastBuffer            

data Change = SavedFilePoint
            | InteractivePoint
            | AtomicChange Update
            deriving Show

changeUpdates :: Change -> [Update]
changeUpdates (AtomicChange u) = [u]
changeUpdates SavedFilePoint = []
changeUpdates InteractivePoint = []


-- | A URList consists of an undo and a redo list.
data URList = URList [Change] [Change]
            deriving Show

-- | A new empty 'URList'.
-- Notice we must have a saved file point as this is when we assume we are
-- opening the file so it is currently the same as the one on disk
emptyUR :: URList
emptyUR = URList [SavedFilePoint] []

-- | Add an action to the undo list.
-- According to the restricted, linear undo model, if we add a command
-- whilst the redo list is not empty, we will lose our redoable changes.
addUR :: Change -> URList -> URList
addUR InteractivePoint ur@(URList (InteractivePoint:_) _) = ur
addUR u (URList us _rs) = URList (u:us) []


-- | Add a saved file point so that we can tell that the buffer has not
-- been modified since the previous saved file point.
-- Notice that we must be sure to remove the previous saved file points
-- since they are now worthless.
setSavedPointUR :: URList -> URList
setSavedPointUR (URList undos redos) =
  URList (SavedFilePoint : cleanUndos) cleanRedos
  where
  cleanUndos = filter isNotSavedFilePoint undos
  cleanRedos = filter isNotSavedFilePoint redos
  isNotSavedFilePoint :: Change -> Bool
  isNotSavedFilePoint SavedFilePoint = False
  isNotSavedFilePoint _              = True

undoInteractivePoint :: URList -> BufferImpl -> (BufferImpl, (URList, [Change]))
undoInteractivePoint (URList (InteractivePoint:cs) rs) b =  (b, (URList cs (InteractivePoint:rs), []))
undoInteractivePoint ur b = (b, (ur, []))

-- | repeat ur action until we find an InteractivePoint.
manyUR :: (URList -> t -> (t, (URList, [a])))-> URList -> t -> (t, (URList, [a]))
manyUR _ ur@(URList [] _) b = (b, (ur, []))
manyUR _ ur@(URList [SavedFilePoint] _) b     = (b, (ur, []))
manyUR _ ur@(URList (InteractivePoint:_) _) b = (b, (ur, []))
manyUR f ur@(URList _ _) b = 
    let (b', (ur', cs')) = f ur b
        (b'', (ur'', cs'')) = manyUR f ur' b'
        in (b'', (ur'', cs'' ++ cs'))

-- | Undo the last action that mutated the buffer contents. The action's
-- inverse is added to the redo list. 
undoUR :: URList -> BufferImpl -> (BufferImpl, (URList, [Change]))
undoUR u@(URList [] _) b                     = (b, (u, []))
undoUR u@(URList [SavedFilePoint] _) b       = (b, (u, []))
undoUR (URList (SavedFilePoint : rest) rs) b = 
  undoUR (URList rest (SavedFilePoint : rs)) b
undoUR (URList (u:us) rs) b                  = 
    let (b', r) = getActionB u b 
    in (b', (URList us (r:rs), [u]))

-- | Redo the last action that mutated the buffer contents. The action's
-- inverse is added to the undo list.
redoUR :: URList -> BufferImpl -> (BufferImpl, (URList, [Change]))
redoUR u@(URList _ []) b = (b, (u, []))
redoUR u@(URList _ [SavedFilePoint]) b = (b, (u, []))
redoUR (URList us (SavedFilePoint : rest)) b =
  redoUR (URList (SavedFilePoint : us) rest) b
redoUR (URList us (r:rs)) b =
    let (b', u) = getActionB r b
    in (b', (URList (u:us) rs, [u]))

-- | isUnchangedUndoList. @True@ if the undo list is either empty or we are at a
-- SavedFilePoint indicated that the buffer has not been modified since we
-- last saved the file.
-- Note: that an empty undo list does NOT mean that the buffer is not modified since
-- the last save. Because we may have saved the file and then undone actions done before
-- the save.
isUnchangedUList :: URList -> Bool
isUnchangedUList (URList (InteractivePoint:us) rs) = isUnchangedUList (URList us rs)
isUnchangedUList (URList [] _)                   = False
isUnchangedUList (URList (SavedFilePoint : _) _) = True
isUnchangedUList (URList _ _)                    = False

-- | Given a Update, apply it to the buffer, and return the
-- Update that reverses it.
getActionB :: Change -> BufferImpl -> (BufferImpl, Change)
getActionB (AtomicChange u) b = (applyUpdateI u (moveToI (updatePoint u) b), 
                                 AtomicChange (reverseUpdate u b))
getActionB c b = (b,c)

-- | Reverse the given update
reverseUpdate :: Update -> BufferImpl -> Update
reverseUpdate (Delete p n)   b  = Insert p (nelemsBI n p b)
reverseUpdate (Insert p cs)  _ = Delete p (length cs)


