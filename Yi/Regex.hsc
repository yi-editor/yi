{-# OPTIONS -fffi -fno-warn-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface to the POSIX regular expression library.
--
-----------------------------------------------------------------------------

--  
-- Regular expression matching, modified to work on buffers.
--

module Yi.Regex (

    -- * The @Regex@ type
    Regex,      -- abstract

    -- * Compiling a regular expression
    regcomp,    -- :: String -> Int -> IO Regex

    -- ** Flags for regcomp
    regExtended,    -- (flag to regcomp) use extended regex syntax
    regIgnoreCase,  -- (flag to regcomp) ignore case when matching
    regNewline,     -- (flag to regcomp) '.' doesn't match newline

    -- * Matching a regular expression
    regexec,    -- :: Regex                 -- pattern
                -- -> Ptr CChar             -- buffer to match on
                -- -> IO (Maybe Int)

  ) where


#include "YiUtils.h"

#include <sys/types.h>
#include <regex.h>

import Foreign
import Foreign.C
import Foreign.ForeignPtr

type CRegex    = ()

-- | A compiled regular expression
newtype Regex = Regex (ForeignPtr CRegex)

-- | Compiles a regular expression
regcomp :: String     -- ^ The regular expression to compile
        -> Int        -- ^ Flags (summed together)
        -> IO Regex   -- ^ Returns: the compiled regular expression

regcomp pattern flags = do
    regex_fptr <- mallocForeignPtrBytes (#const sizeof(regex_t))
    r <- withCString pattern $ \cstr ->
        withForeignPtr regex_fptr $ \p ->
            c_regcomp p cstr (fromIntegral flags)
    if (r == 0)
        then do
#if !defined(__NHC__) 
# if __GLASGOW_HASKELL__ >= 602
             addForeignPtrFinalizer ptr_regfree regex_fptr
# else
             addForeignPtrFinalizer regex_fptr ptr_regfree  -- swapped
# endif 
#else
             addForeignPtrFinalizer regex_fptr (return ()) -- :: ForeignPtr a -> IO () -> IO ()
#endif
             return (Regex regex_fptr)
        else ioError $ userError $ "Error in pattern: " ++ pattern

-- regexec

-- | Matches a regular expression against a buffer, returning the buffer
-- index of the match
--
regexec :: Regex                -- ^ Compiled regular expression
        -> Ptr CChar            -- ^ The buffer to match against
        -> Int                  -- ^ Offset in buffer to start searching from
        -> IO (Maybe (Int,Int)) -- ^ Returns: 'Nothing' if the regex did not match the
                                -- or Just the start and end indicies of the match.

regexec (Regex regex_fptr) ptr i = do
    withForeignPtr regex_fptr $ \regex_ptr -> do
        nsub <- (#peek regex_t, re_nsub) regex_ptr
        let nsub_int = fromIntegral (nsub :: CSize)
        allocaBytes ((1 + nsub_int)*(#const sizeof(regmatch_t))) $ \p_match-> do
            -- add one because index zero covers the whole match
            r <- cregexec regex_ptr (ptr `plusPtr` i) 
                                    (1 + nsub) p_match 0{-no flags -}

            if (r /= 0) then return Nothing else do 
                is <- matched_parts p_match
                return (Just is)

matched_parts :: Ptr CRegMatch -> IO (Int, Int)
matched_parts p_match = do
    start <- (#peek regmatch_t, rm_so) p_match :: IO (#type regoff_t)
    end   <- (#peek regmatch_t, rm_eo) p_match :: IO (#type regoff_t)
    let s = fromIntegral start; e = fromIntegral end
    return (s, e)

-- -----------------------------------------------------------------------------
-- The POSIX regex C interface

-- Flags for regexec
#enum Int,, \
    REG_NOTBOL, \
    REG_NOTEOL

-- Return values from regexec
#enum Int,, \
    REG_NOMATCH
--  REG_ESPACE

-- Flags for regcomp
#enum Int,, \
    REG_EXTENDED, \
    regIgnoreCase = REG_ICASE, \
    REG_NOSUB, \
    REG_NEWLINE

-- Error codes from regcomp
#enum Int,, \
    REG_BADBR, \
    REG_BADPAT, \
    REG_BADRPT, \
    REG_ECOLLATE, \
    REG_ECTYPE, \
    REG_EESCAPE, \
    REG_ESUBREG, \
    REG_EBRACK, \
    REG_EPAREN, \
    REG_EBRACE, \
    REG_ERANGE, \
    REG_ESPACE

type CRegMatch = ()

foreign import ccall unsafe "regcomp"
    c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "YiUtils.h &regfree"
    ptr_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regexec"
    cregexec :: Ptr CRegex 
             -> Ptr CChar 
             -> CSize -> Ptr CRegMatch -> CInt -> IO CInt

-- ---------------------------------------------------------------------
-- Some stuff missing from the nhc98-1.16 libs
-- Watch that memory leak..
--
#ifdef __NHC__

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes n = do
    r <- mallocBytes n
    newForeignPtr r (return ()) -- :: Ptr a -> IO () -> IO (ForeignPtr a)

type FinalizerPtr a        = FunPtr            (Ptr a -> IO ())

#endif
