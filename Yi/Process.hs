{-# OPTIONS -cpp #-}
--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- | A Posix.popen compatibility mapping.
-- Based on PosixCompat, originally written by Derek Elkins for lambdabot
--
module Yi.Process (popen) where

#if __GLASGOW_HASKELL__ >= 604
import System.IO
import System.Process
import Control.Concurrent       (forkIO)
#else
import qualified Posix as P
#endif

import qualified Control.Exception

#if __GLASGOW_HASKELL__ >= 604

type ProcessID = ProcessHandle

popen :: FilePath -> [String] -> Maybe String -> IO (String,String,ProcessID)
popen file args minput =
    Control.Exception.handle (\e -> return ([],show e,error (show e))) $ do

    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing

    case minput of
        Just input -> hPutStr inp input >> hClose inp -- importante!
        Nothing    -> return ()

    -- Now, grab the input
    output <- hGetContents out
    errput <- hGetContents err

    -- SimonM sez:
    --  ... avoids blocking the main thread, but ensures that all the
    --  data gets pulled as it becomes available. you have to force the
    --  output strings before waiting for the process to terminate.
    --
    forkIO (Control.Exception.evaluate (length output) >> return ())
    forkIO (Control.Exception.evaluate (length errput) >> return ())

    -- And now we wait. We must wait after we read, unsurprisingly.
    waitForProcess pid -- blocks without -threaded, you're warned.

    -- so what's the point of returning the pid then?
    return (output,errput,pid)

#else

--
-- catch so that we can deal with forkProcess failing gracefully.  and
-- getProcessStatus is needed so as not to get a bunch of zombies,
-- leading to forkProcess failing.
--
-- Large amounts of input will cause problems with blocking as we wait
-- on the process to finish. Make sure no lambdabot processes will
-- generate 1000s of lines of output.
--
popen :: FilePath -> [String] -> Maybe String -> IO (String,String,P.ProcessID)
popen f s m =
    Control.Exception.handle (\e -> return ([], show e, error $ show e )) $ do
        x@(_,_,pid) <- P.popen f s m
        b <- P.getProcessStatus True False pid  -- wait
        return $ case b of
            Nothing -> ([], "process has disappeared", pid)
            _       -> x

#endif
