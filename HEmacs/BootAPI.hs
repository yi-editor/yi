{-# OPTIONS -fglasgow-exts #-}
--
-- riot/BootAPI.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- this is the only module that is linked twice, once statically linked,
-- and once dynamically. It must be done so that the type of the value
-- passed from Boot.main to Main.main is available on both sides of the
-- border. We use the existential to prevent a dependency on ConfigAPI
-- in Boot.hs. It gets unwrapped magically in Main.dynamic_main
--
module BootAPI where

data ConfigData = forall a. CD a {- has Config type -}

type HEmacsMainType = (Maybe ConfigData) -> IO ()

