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
        emptyUR,
        addUR,
        undoUR,
        redoUR,
        isEmptyUList,
        addBoundary,
        getActionB,
        URList,             {- abstractly -}
        URAction(..),       {- non-abstractly, for concrete implementations -}
   ) where

import Yi.FastBuffer            

--
-- | A URList consists of an undo and a redo list.
--
data URList = URList ![URAction] ![URAction]

--
-- | Mutation actions (from the undo or redo list)
--
-- We use the /partial checkpoint/ (Berlage, pg16) strategy to store
-- just the components of the state that change.
--
-- The state (i.e. editor contents) are stored CChars, this has
-- implications for Unicode.
--
data URAction = Insert !Point !String -- FIXME: use ByteString
              | Delete !Point !Size
      --      | Boundary
                deriving Show


-- | A new empty 'URList'.
emptyUR :: URList
emptyUR = URList [] []

--
-- | Add an action to the undo list.
-- According to the restricted, linear undo model, if we add a command
-- whilst the redo list is not empty, we will lose our redoable changes.
addUR :: URList -> URAction -> URList
addUR (URList us _rs) u =
    URList (u:us) []

--
-- | Undo the last action that mutated the buffer contents. The action's
-- inverse is added to the redo list. 
undoUR :: BufferImpl -> URList -> IO (URList, [URAction])
undoUR _ u@(URList [] _) = return (u, [])
undoUR b (URList (u:us) rs) = do
    r <- (getActionB u) b
    return (URList us (r:rs), [u])

--
-- | Redo the last action that mutated the buffer contents. The action's
-- inverse is added to the undo list.
redoUR :: BufferImpl -> URList -> IO (URList, [URAction])
redoUR _ u@(URList _ []) = return (u, [])
redoUR b (URList us (r:rs)) = do
    u <- (getActionB r) b
    return (URList (u:us) rs, [u])


-- | isEmptyUndoList. @True@ if the undo list is empty, and hence the
-- buffer is not modified
isEmptyUList :: URList -> Bool
isEmptyUList (URList [] _) = True
isEmptyUList (URList _  _) = False

-- | Add an undo `boundary', for save-points and the like
addBoundary :: URList -> URList
addBoundary = undefined



-- | Given a URAction, apply it to the buffer, and return the
-- URAction that reverses it.
--
getActionB :: URAction -> BufferImpl -> IO URAction
getActionB (Delete p n) b = do
    moveToI p b
    p' <- pointBI b
    text <- nelemsBI n p' b
    deleteNAtI b n p'
    return $ Insert p' text

getActionB (Insert p cs) b = do
    moveToI p b
    insertNI b cs
    return $ Delete p (length cs)

