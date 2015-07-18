module Shared.Utils (
    applyToPointer,
    nullPtr,
    with,
    with2,
    withCAString2,
    ScreenSize
) where

import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable


type ScreenSize = (CInt, CInt)

applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer

withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b op = withCAString a $ \a' -> withCAString b $ op a'

with2 :: (Storable a, Storable b) => a -> b -> (Ptr a -> Ptr b -> IO c) -> IO c
with2 a b op = with a $ \a' -> with b $ op a'
