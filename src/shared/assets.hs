module Shared.Assets where

import qualified Graphics.UI.SDL as SDL
import GHC.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Shared.Lifecycle

type RenderOperation = Ptr SDL.Surface -> IO CInt

loadBitmap :: String -> IO (Either String (Ptr SDL.Surface))
loadBitmap path = do
    surface <- withCAString path SDL.loadBMP
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface

getSurfaceFrom :: FilePath -> IO (Ptr SDL.Surface)
getSurfaceFrom path = loadBitmap path >>= either throwSDLError return

optimizeSurface :: Ptr SDL.Surface -> Ptr SDL.PixelFormat -> Word32 -> IO (Either String (Ptr SDL.Surface))
optimizeSurface surface format flags = do
    optimizedSurface <- SDL.convertSurface surface format flags
    return $ if optimizedSurface == nullPtr then Left "Unable to optimize image!" else Right optimizedSurface

drawOn :: SDL.Window -> Ptr SDL.Surface -> RenderOperation
drawOn window screen surface = SDL.blitSurface surface nullPtr screen nullPtr >> SDL.updateWindowSurface window

