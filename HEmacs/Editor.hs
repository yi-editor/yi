--
-- riot/Editor.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module HEmacs.Editor (
    edittext,
    editfile,
    get_editor
 ) where

import HEmacs.Version                   ( package )
import HEmacs.MkTemp                    ( mkstemp )

import System.IO
import System.Environment               ( getEnv )
import System.Cmd                       ( system )

#ifndef NO_POSIX
import System.Posix.Files               ( removeLink )
import System.Posix.Unistd
#endif

shell_safe_string s =
    concat ["'", escape s, "'"]
    where
        escape [] = []
        escape ('\'':s) = "\\'"++escape s
        escape (x:s) = x:escape s

my_system cmd args =
    system $ concat (cmd:map (\s -> " "++shell_safe_string s) args)


--import Config(preferred_editor, fallback_editor)
preferred_editor = Nothing -- Use environment
fallback_editor = "vi" -- If environment fails

get_editor = 
    case preferred_editor of
        Just e -> return e
        Nothing -> catch (getEnv "VISUAL")
                         (\_ -> catch (getEnv "EDITOR")
                                      (\_ -> return fallback_editor))

editfile fname = do
    editor <- get_editor
    my_system editor [fname]

--
-- Try to create a temp file. Should handle possible errors more
-- gracefully
--
make_temp :: IO (FilePath, Handle)
make_temp = do 
       mf <- mkstemp $ "/tmp/"++package++"-XXXXXXXXXX"
       case mf of 
                Nothing -> error "Couldn't create tmp file"
                Just v  -> return v
                

edittext text = do
    (fname, h) <- make_temp
    catch (do_edittext fname h text)
          (\e -> finish fname h >> ioError e)
    where
        finish fname h = hClose h >> removeLink fname
        do_edittext fname h text = do
            hPutStr h text
            hClose h
            editfile fname
            txt <- readFile fname
            removeLink fname
            return txt

