{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-- |
-- Utilities for primitive marshalling of C strings.
--
-- The marshalling converts each Haskell character, representing a Unicode
-- code point, to one or more bytes in a manner that, by default, is
-- determined by the current locale.  As a consequence, no guarantees
-- can be made about the relative length of a Haskell string and its
-- corresponding C string, and therefore all the marshalling routines
-- include memory allocation.  The translation between Unicode and the
-- encoding of the current locale may be lossy.

module Foreign.C.String (

  -- * C strings

  CString,           -- = Ptr CChar
  CStringLen,        -- = (Ptr CChar, Int)

  -- ** Using a locale-dependent encoding

  -- | Currently these functions are identical to their @CAString@ counterparts;
  -- eventually they will use an encoding determined by the current locale.

  -- conversion of C strings into Haskell strings
  --
  peekCString,       -- :: CString    -> IO String
  peekCStringLen,    -- :: CStringLen -> IO String

  -- conversion of Haskell strings into C strings
  --
  newCString,        -- :: String -> IO CString
  newCStringLen,     -- :: String -> IO CStringLen

  -- conversion of Haskell strings into C strings using temporary storage
  --
  withCString,       -- :: String -> (CString    -> IO a) -> IO a
  withCStringLen,    -- :: String -> (CStringLen -> IO a) -> IO a

  charIsRepresentable, -- :: Char -> IO Bool

  -- ** Using 8-bit characters

  -- | These variants of the above functions are for use with C libraries
  -- that are ignorant of Unicode.  These functions should be used with
  -- care, as a loss of information can occur.

  castCharToCChar,   -- :: Char -> CChar
  castCCharToChar,   -- :: CChar -> Char

  castCharToCUChar,  -- :: Char -> CUChar
  castCUCharToChar,  -- :: CUChar -> Char
  castCharToCSChar,  -- :: Char -> CSChar
  castCSCharToChar,  -- :: CSChar -> Char

  peekCAString,      -- :: CString    -> IO String
  peekCAStringLen,   -- :: CStringLen -> IO String
  newCAString,       -- :: String -> IO CString
  newCAStringLen,    -- :: String -> IO CStringLen
  withCAString,      -- :: String -> (CString    -> IO a) -> IO a
  withCAStringLen,   -- :: String -> (CStringLen -> IO a) -> IO a

  -- * C wide strings

  -- | These variants of the above functions are for use with C libraries
  -- that encode Unicode using the C @wchar_t@ type in a system-dependent
  -- way.  The only encodings supported are
  --
  -- * UTF-32 (the C compiler defines @__STDC_ISO_10646__@), or
  --
  -- * UTF-16 (as used on Windows systems).

  CWString,          -- = Ptr CWchar
  CWStringLen,       -- = (Ptr CWchar, Int)

  peekCWString,      -- :: CWString    -> IO String
  peekCWStringLen,   -- :: CWStringLen -> IO String
  newCWString,       -- :: String -> IO CWString
  newCWStringLen,    -- :: String -> IO CWStringLen
  withCWString,      -- :: String -> (CWString    -> IO a) -> IO a
  withCWStringLen,   -- :: String -> (CWStringLen -> IO a) -> IO a

  ) where
import "base" Foreign.C.String

