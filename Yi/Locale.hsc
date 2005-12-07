{-# OPTIONS -fffi -fglasgow-exts #-}
--
-- Copyright (c) 2002-2004 John Meacham (john at repetae dot net)
-- Copyright (c) 2004      Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- arch-tag: 72067bff-05e1-4c0e-94aa-34b54f437d92
--

-- ToDo Addr## should be a more ffi-friendly type

--
-- arch-tag: d48a3194-c698-43c7-b581-08e7a213f0c8
--

module Yi.Locale (

        setupLocale,
        -- getCharset,
        getDateFmt, getDateTimeFmt, getTimeFmt,
        getYesRegex, getNoRegex
        -- nl_langinfo

    ) where

import Foreign      ( Int32 )
import Foreign.C    ( peekCString, CChar, CInt )
import GHC.Exts

#include "config.h"
#include <locale.h>
#include <langinfo.h>

foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> Addr## -> IO (Ptr CChar)

foreign import ccall unsafe "langinfo.h nl_langinfo"
    nl_langinfo :: (#type nl_item) -> IO (Ptr CChar)

setupLocale :: IO ()
setupLocale = setlocale (#const LC_ALL) ""## >> return ()

getDateFmt :: IO String
getDateFmt = nl_langinfo (#const D_FMT) >>= peekCString

getDateTimeFmt :: IO String
getDateTimeFmt = nl_langinfo (#const D_T_FMT) >>= peekCString

getTimeFmt :: IO String
getTimeFmt = nl_langinfo (#const T_FMT) >>= peekCString

getYesRegex :: IO String
getYesRegex = nl_langinfo (#const YESEXPR) >>= peekCString

getNoRegex :: IO String
getNoRegex = nl_langinfo (#const NOEXPR) >>= peekCString

{-
getCharset :: IO String
#ifndef NO_LANGINFO_CODESET
getCharset =  nl_langinfo (#const CODESET) >>= peekCString
#else
getCharset =  error "Ginsu.Locale.getCharset: not defined for this platform"
#endif
-}

{-
LC_COLLATE
    Affects the behavior of regular expressions and the collation functions.
LC_CTYPE
    Affects the behavior of regular expressions, character classification,
    character conversion functions, and wide-character functions.
LC_MESSAGES
     Affects what strings are expected by commands and utilities as affirmative
     or negative responses.  It also affects what strings are given by commands
     and utilities as affirmative or negative responses, and the content of
     messages.
LC_MONETARY
    Affects the behavior of functions that handle monetary values.
LC_NUMERIC
    Affects the behavior of functions that handle numeric values.
LC_TIME
    Affects the behavior of the time conversion functi

data Locale = LC_CTYPE | LC_NUMERIC | LC_TIME | LC_COLLATE | LC_MONETARY | LC_MESSAGES | LC_ALL | LC_PAPER | LC_NAME | LC_ADDRESS | LC_TELEPHONE | LC_MEASUREMENT | LC_IDENTIFICATION
    deriving(Show, Enum, Read, Ord, Eq)

decodeLocale :: Locale -> CInt
decodeLocale LC_CTYPE = (#const LC_CTYPE)
decodeLocale LC_NUMERIC = (#const LC_NUMERIC)
decodeLocale LC_COLLATE = (#const LC_COLLATE)
decodeLocale LC_MONETARY = (#const LC_MONETARY)
decodeLocale LC_TIME = (#const LC_TIME)
#ifdef LC_MESSAGES
decodeLocale LC_MESSAGES = (#const LC_MESSAGES)
#endif
#ifdef LC_PAPER
decodeLocale LC_PAPER = (#const LC_PAPER)
#endif
#ifdef LC_NAME
decodeLocale LC_NAME = (#const LC_NAME)
#endif
#ifdef LC_ADDRESS
decodeLocale LC_ADDRESS = (#const LC_ADDRESS)
#endif
#ifdef LC_TELEPHONE
decodeLocale LC_TELEPHONE = (#const LC_TELEPHONE)
#endif
#ifdef LC_MEASUREMENT
decodeLocale LC_MEASUREMENT = (#const LC_MEASUREMENT)
#endif
#ifdef LC_IDENTIFICATION
decodeLocale LC_IDENTIFICATION = (#const LC_IDENTIFICATION)
#endif
decodeLocale _ = -1

dString :: IO (Ptr CChar) -> IO (Maybe String)
dString action = do
    v <- action
    if v == nullPtr then return Nothing else fmap Just (peekCString v)

setLocaleAll :: String -> IO (Maybe String)
setLocaleAll s =  dString $ withCString s (setlocale (#const LC_ALL))

getLocaleAll :: IO (Maybe String)
getLocaleAll = getLocale LC_ALL

setLocale :: Locale -> String -> IO (Maybe String)
setLocale l s = case decodeLocale l of
    -1 -> return Nothing
    nl -> dString $ withCString s (setlocale nl)

getLocale :: Locale -> IO (Maybe String)
getLocale l = case decodeLocale l of
    -1 -> return Nothing
    nl -> dString $ setlocale nl nullPtr
-}
