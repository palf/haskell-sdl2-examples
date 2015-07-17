module Shared.Utils (
    applyToPointer,
    nullPtr,
    with,
    ScreenSize
) where

import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable


type ScreenSize = (CInt, CInt)

applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer
