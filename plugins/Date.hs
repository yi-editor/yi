--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Config where

import HEmacs.ConfigAPI

import System.IO
import System.IO.Unsafe     ( unsafePerformIO )
import System.Posix.Types   ( ProcessID, Fd )
import System.Posix.Process ( forkProcess, executeFile, getProcessID )
import System.Posix.IO      ( createPipe, stdInput, 
                              stdOutput, fdToHandle, closeFd, dupTo )

import qualified HEmacs.Curses as Curses ( scrSize )

------------------------------------------------------------------------

hemacs = settings {
    topinfo_text = date
}

------------------------------------------------------------------------
--
-- reimplementation of Posix.popen, to avoid a dependency on -package posix
--

date :: String
date = unsafePerformIO $ do
        (_,w) <- Curses.scrSize
        (hdl,_,_) <- catch (popen "/bin/date") (\_ -> error "popen failed")
        s <- hGetLine hdl
        hClose hdl
        return $ (take (w - (1 + length s)) $ repeat ' ') ++ s

--
-- my implementation of $val = `cmd`; (if this was perl)
--

popen :: FilePath -> IO (Handle, Handle, ProcessID)
popen cmd = do
        (pr, pw) <- createPipe
        (cr, cw) <- createPipe    

        -- parent --
        let parent = do closeFd cw
                        closeFd pr
        -- child --
        let child  = do closeFd pw
                        closeFd cr 
                        exec cmd (pr,cw)
                        error "exec cmd failed!" -- typing only

        pid <- forkProcess child -- fork child
        parent                   -- and run parent code
        hcr <- fdToHandle cr
        hpw <- fdToHandle pw
        return (hcr,hpw,pid)

--
-- execve cmd in the child process, dup'ing the file descriptors passed
-- as arguments to become the child's stdin and stdout.
--
exec :: FilePath -> (Fd,Fd) -> IO ()
exec cmd (pr,cw) = do
        dupTo pr stdInput
        dupTo cw stdOutput
        executeFile cmd False [] Nothing
