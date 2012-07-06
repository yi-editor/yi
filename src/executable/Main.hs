-- Copyright (C) 2008 JP Bernardy
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (C) 2012 Corey O'Connor
--
-- | This process manages the compilation and execution of yi.
--
-- The management process is modeled as a state machine. The initial state is process start and the
-- final states are process termination.
--

module Main (main) where

import Prelude hiding ( catch )

import Control.Applicative
import Control.Exception

import System.Directory
import System.Exit
import System.IO

import Text.Printf

appDir :: IO FilePath
appDir = getAppUserDataDirectory "yi"

info :: HPrintfType r => String -> r
info format = hPrintf stderr format

data YiSystem where
    Init :: YiSystem
    Die  :: Int -> String -> YiSystem
    Exit :: YiSystem

data SystemConfig where
    SystemConfig ::
        { hasUserConfig :: Bool
        } -> SystemConfig
    deriving ( Show, Eq, Read )

main :: IO ()
main = catch (manageYiSystem Init) dieFromException

dieFromException :: SomeException -> IO a
dieFromException e = manageYiSystem (Die 1 $ show e)

manageYiSystem :: YiSystem -> IO a
manageYiSystem (Die n reason) = do
    info "E%d - %s" n reason
    exitWith $ ExitFailure n
manageYiSystem Exit = do
    exitWith ExitSuccess
manageYiSystem Init = do
    -- Determine what we know about the Yi system
    user_dir_exists <- doesDirectoryExist <$> appDir
    manageYiSystem Exit

