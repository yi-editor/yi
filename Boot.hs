--
-- yi/Boot.hs
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
-- | Boot loader for yi.
-- This is a small stub that loads the runtime yi library, then
-- jumps to it. It solves the problem of unnecessary code linking.  As
-- such, we only want to do plugin-related stuff here. So we don't even
-- mess with the locale stuff yet. Maybe not so small since it is
-- statically linked against -package plugins :(
--
-- We should have no static dependencies on any other part of yi.
--
-- One of our jobs is to find a (value :: Config) to pass to
-- Yi.dynamic_main (a dynamically loaded module in the yi
-- library). To do this we need to check if the -B flag was on the
-- command line.
--
-- Once we find our Config file we have to:
--      * load Config.o if it exists
--      * load -package yi, and any dependencies, through Yi.o
--      * jump to Yi.dynamic_main with v:: Config as an argument
--
-- This is the only module that depends on -package plugins (at the
-- moment -- if we want to reload() based on a user action, we'll have
-- to load plugins dynamically too).
--
-- NB: ncurses must be statically linked in. It can't be dynamically loaded
--
-- NB: we need to distribute Yi.o outside of HSyi.o, it is our
-- hs-plugins.load entry point to HSyi.o. Loading Yi.o pulls in
-- HSyi.o (i.e. -package yi) as a dependency.
--

module Boot ( main ) where

import Plugins
import Plugins.Utils          ( (</>), (<.>) )

import Data.Maybe             ( fromJust, isJust )
import Data.IORef             ( newIORef, readIORef, writeIORef, IORef() )
import Control.Monad          ( when )
import System.IO              ( hFlush, stdout )
import System.Directory
import System.Console.GetOpt
import System.IO.Unsafe       ( unsafePerformIO )
import System.Environment     ( getArgs, getEnv )
import System.Exit            ( exitFailure )
import qualified Control.Exception ( catch )

#ifndef NO_POSIX
import System.Posix.User      ( getUserEntryForID, getRealUserID, homeDirectory )
#endif

-- ---------------------------------------------------------------------
-- yi uses config scripts stored in ~/.yi
--

config_dir, config_file, yi_main_obj :: FilePath
config_dir      = ".yi"                 -- ~/.yi/ stores Config.hs
config_file     = "Config.hs"           -- name of user-defineable Config file

yi_main_obj     = "Yi.o"                -- entry point into yi lib

config_sym, yi_main_sym :: Symbol
config_sym      = "yi"                  -- symbol to retrieve from Config.hs
yi_main_sym     = "dynamic_main"        -- main entry point

-- ---------------------------------------------------------------------
-- | Where do the libraries live?
-- This value can be overridden on the command line with the -B flag
--
libdir :: IORef FilePath
libdir = unsafePerformIO $ newIORef (LIBDIR :: FilePath)
{-# NOINLINE libdir #-}

-- ---------------------------------------------------------------------
-- | Finding config files. Use 'Control.Exception.catch' to deal with
-- broken or missing implementations of get functions.
--
get_home :: IO String
#ifndef NO_POSIX
get_home = Control.Exception.catch 
        (getRealUserID >>= getUserEntryForID >>= (return . homeDirectory))
        (\_ -> getEnv "HOME")
#else
get_home = error "Boot.get_home not defined for this platform"
#endif

-- | ~/.yirc/
get_config_dir  :: IO String
get_config_dir = do
    home <- get_home
    return $ home</>config_dir

-- | ~/.yirc/Config.hs
get_config_file :: IO (String)
get_config_file = do
    home <- get_home
    return $ home</>config_dir</>config_file

-- ---------------------------------------------------------------------
-- Do some argv parsing to detect any -B args needed to find our runtime
-- libraries. Any other args are ignored and passed through to
-- Yi.main. Think like GHCs +RTS -RTS options, except we don't
-- remove -B flags -- we expect them to be ignored by Yi.main
--

data Opts = LibDir FilePath

options :: [OptDescr Opts]
options = [
        Option ['B']  ["libdir"]  (ReqArg LibDir "libdir") "Path to runtime libraries"
    ]

--
-- Determine if there is a libdir -B flag provided
--
doArgs :: [String] -> (Maybe FilePath)
doArgs argv = case getOpt Permute options argv of
        (o, _, _) -> case reverse o of
                        []           -> Nothing
                        (LibDir d:_) -> Just d

-- ---------------------------------------------------------------------
-- Given a source file, compile it to a (.o, .hi) pair
-- Jump to the ~/.yi/ directory, in case we are running in-place, to
-- prevent bogus module dependencies.
--
-- NB need hs-plugins >= Oct 26 2004, due to bug in recompilation checking
--
compile :: FilePath -> IO (Maybe FilePath)
compile src = do
    build_dir <- get_config_dir
    old_pwd   <- getCurrentDirectory 
    setCurrentDirectory build_dir

    flags  <- get_make_flags
    status <- makeAll src flags

    setCurrentDirectory old_pwd

    case status of
        MakeSuccess _ obj -> return $ Just obj
        MakeFailure errs  -> do 
            putStrLn "Errors in config file, using defaults"
            mapM_ putStrLn errs
            return Nothing

--
-- What packages do we need to build Config.hs?
--
packages :: [String]
packages = [ "yi" ]

--
-- Flags to find the runtime libraries, to help ghc out
--
-- Grr... ghc-6.2.1 and less have the annoying behaviour of, when given
-- the choice between linking an object from an explicit -package flag,
-- or from a path in the current directory, always chooses the latter.
-- This results in explicit module dependencies, rather than package
-- dependencies. Bad from dynamic loading when running in-place.
--
-- A way around this is to get to cd to another directory where the -i.
-- flag means nothing.
--
get_make_flags :: IO [String]
get_make_flags = do libpath <- readIORef libdir
                    return $! concatMap (f libpath) packages
    where 
        f l p = ["-package-conf", l </> p <.> "conf", "-package", p]

get_load_flags :: IO [String]
get_load_flags = do libpath <- readIORef libdir
                    return $ map (\p -> libpath </> p <.> "conf") packages

-- ---------------------------------------------------------------------
-- | Find and load a config file. Load the yi core library. Jump to
-- the real main in 'Yi.main', passing any config information we found.
--
main :: IO ()
main = do
    -- look for -B libdir flag
    argv <- getArgs              
    let mlib = doArgs argv 
    when (isJust mlib) $ writeIORef libdir (fromJust mlib)

    -- check if ~/.yi/ exists
    d        <- get_config_dir          
    d_exists <- doesDirectoryExist d
    when (not d_exists) $ createDirectory d

    putStr "Starting up dynamic Haskell ... " >> hFlush stdout

    -- look for ~/.yi/Config.hs
    c        <- get_config_file 
    c_exists <- doesFileExist c
    m_obj    <- if c_exists then compile c else return Nothing
 
    -- now load user's Config.o if we have it
    paths   <- get_load_flags
    libpath <- readIORef libdir
    cfghdl  <- case m_obj of
        Nothing  -> return Nothing
        Just obj -> do status <- load obj [d,libpath] paths config_sym
                       case status of
                            LoadSuccess _ v -> return $ Just (CD v)
                            LoadFailure e   -> do
                                putStrLn "Unable to load config file, using defaults"
                                mapM_ putStrLn e ; return Nothing

    -- now, get a handle to Main.dynamic_main, and jump to it
    status    <- load (libpath </> yi_main_obj) [] paths yi_main_sym
    yi_main <- case status of
        LoadSuccess _ v -> return (v :: YiMainType)
        LoadFailure e -> do putStrLn "Unable to load Yi.Main, exiting"
                            mapM_ putStrLn e ; exitFailure

    putStr "jumping over the edge ... " >> hFlush stdout
    yi_main cfghdl   -- jump to dynamic code

-- ---------------------------------------------------------------------
-- | MAGIC: this is the type of the value passed from Boot.main to
-- Yi.main. It must be exactly the same as the definition in
-- Yi.hs.  We can't, however, share the value in another module,
-- without breaking ghci support (the same module would be linked both
-- statically and dynamically). 
--
-- It is an existential to prevent a dependency on ConfigAPI in Boot.hs.
-- It gets unwrapped magically in Yi.dynamic_main
--
data ConfigData = forall a. CD a {- has Config type -}

type YiMainType = (Maybe ConfigData) -> IO ()

-- vim: sw=4 ts=4
