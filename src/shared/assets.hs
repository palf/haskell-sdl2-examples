module Shared.Assets where

import qualified Graphics.UI.SDL as SDL
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

drawOn :: SDL.Window -> Ptr SDL.Surface -> RenderOperation
drawOn window screen surface = SDL.blitSurface surface nullPtr screen nullPtr >> SDL.updateWindowSurface window
