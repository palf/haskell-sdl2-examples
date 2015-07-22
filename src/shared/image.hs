module Shared.Image where


import qualified Graphics.UI.SDL as SDL
import Shared.Lifecycle

import Foreign.Ptr
import Foreign.C.String

getSurfaceFrom' :: FilePath -> IO (Ptr SDL.Surface)
getSurfaceFrom' path = imgLoadSurface path >>= either throwSDLError return

imgLoadSurface :: String -> IO (Either String (Ptr SDL.Surface))
imgLoadSurface file
    = withCString file $ \cFile -> do
        tex <- _imgLoad cFile
        if tex == nullPtr
            then return (Left "IMG_LoadSurface(): Unknown error!")
            else return (Right tex)
foreign import ccall unsafe "IMG_Load" _imgLoad :: CString -> IO (Ptr SDL.Surface)

