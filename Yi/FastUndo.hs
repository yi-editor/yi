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
-- General undo/redo support. Based on proposal by sjw.
--
-- Big deletes will be a problem.
--

module Yi.FastUndo ( 
        URList, {- abstract -} 
        URAction(..),
        emptyUR,    -- :: URList
        addUR,      -- :: URList -> URAction -> URList
        undoUR,     -- :: Buffer a => a -> URList -> IO URList
        redoUR,     -- :: Buffer a => a -> URList -> IO URList
    ) where

import Yi.Buffer

--
-- | A URList consists of an undo and a redo list.
--
data URList = URList ![URAction] ![URAction]

--
-- Mutation actions (from the undo or redo list) are either inserts or
-- deletions
--
data URAction = Insert !Int !Char
              | Delete !Int

-- ---------------------------------------------------------------------
-- | Create a new 'URList'.
emptyUR :: URList
emptyUR = URList [] []

--
-- | Add an action to the undo list. The first argument is either a
-- @Left@ buffer point and 'Char' to insert, or @Right@, a point from
-- which to delete a character.
addUR :: URList -> URAction -> URList
addUR (URList us rs) u = URList (u:us) rs

--
-- | Undo the last action that mutated the buffer contents. The action's
-- inverse is added to the redo list.
--
undoUR :: Buffer a => a -> URList -> IO URList
undoUR _ u@(URList [] _) = return u
undoUR b (URList (u:us) rs) = do
    let f = getAction u
    r <- f b
    return (URList us (r:rs))

--
-- | Redo the last action that mutated the buffer contents. The action's
-- inverse is added to the undo list.
--
redoUR :: Buffer a => a -> URList -> IO URList
redoUR _ u@(URList _ []) = return u
redoUR buf (URList us (r:rs)) = do
    u <- (getAction r) buf
    return (URList (u:us) rs)

-- ---------------------------------------------------------------------
-- INTERNAL:
--
-- Given a URAction, return the buffer action it represents, and the 
-- URAction that reverses it.
--
getAction :: Buffer a => URAction -> (a -> IO URAction) 
getAction (Delete p) b = do 
    moveTo b p
    c <- readB b
    deleteB b
    return (Insert p c)

getAction (Insert p c) b = do
    moveTo b p
    insertB b c
    return (Delete p)

