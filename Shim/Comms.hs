{-# LANGUAGE CPP #-}
module Shim.Comms where
{-
  Communications channel for shim.
  Uses Unix domain sockets where possible and a loopback TCP connection
  otherwise.
 -}


import System
import System.IO
import Shim.Utils

#ifdef mingw32_HOST_OS

-- On windows there are no unix domain sockets.
-- We use a TCP socket on loopback instead.
import Network.Socket

listenOnLoop :: PortNumber -> IO Socket
listenOnLoop port = do
    sock <- socket AF_INET Stream 0
    loopback <- inet_addr "127.0.0.1"
    bindSocket sock (SockAddrInet port loopback)
    setSocketOption sock ReuseAddr 1
    listen sock 5
    return sock

withServerConnection :: String -> (Handle -> IO a) -> IO a
withServerConnection portStr act = withSocketsDo $ (do
    sock <- listenOnLoop (fromIntegral (read portStr :: Integer))
    (sock2, _) <- accept sock
    sClose sock
    h <- socketToHandle sock2 ReadWriteMode
    act h)

#else

-- Unix uses Unix-domain sockets.
import Network
import System.Posix
import Control.Exception ( bracket )

withServerConnection :: String -> (Handle -> IO a) -> IO a
withServerConnection socketfile act = withSocketsDo $ bracket
  (listenOn (UnixSocket socketfile))
  (\_ -> do removeLink socketfile
            removeLink =<< getLogfile)
  (\sock -> do (h,_,_) <- accept sock
               act h)
#endif

