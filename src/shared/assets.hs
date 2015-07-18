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

{-
loadTexture :: SDL.Renderer -> String -> IO (Either String SDL.Texture)
loadTexture renderer path = Imdo
    loadedSurface <- getSurfaceFrom path
    newTexture <- createTextureFromSurface renderer loadedSurface >>= either throwSDLError return
    SDL.freeSurface loadedSurface
    return $ if newTexture == nullPtr then Left "failed to load texture image" else Right newTexture

createTextureFromSurface :: SDL.Renderer -> Ptr SDL.Surface -> IO (Either String SDL.Texture)
createTextureFromSurface renderer surface = do
    result <- SDL.createTextureFromSurface renderer surface
    return $ if result == nullPtr then Left "Unable to create texture" else Right result
-}

drawOn :: SDL.Window -> Ptr SDL.Surface -> RenderOperation
drawOn window screen surface = SDL.blitSurface surface nullPtr screen nullPtr >> SDL.updateWindowSurface window

