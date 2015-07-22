module Shared.Textures where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign.C.Types
import Foreign.Ptr
import Shared.Lifecycle
import Shared.Utils

{-
loadTexture :: SDL.Renderer -> String -> IO (Either String SDL.Texture)
loadTexture renderer path = Imdo
    loadedSurface <- getSurfaceFrom path
    newTexture <- createTextureFromSurface renderer loadedSurface >>= either throwSDLError return
    SDL.freeSurface loadedSurface
    return $ if newTexture == nullPtr then Left "failed to load texture image" else Right newTexture
-}

destroyTextures :: [SDL.Texture] -> IO ()
destroyTextures = mapM_ SDL.destroyTexture

loadTexture :: SDL.Renderer -> String -> IO SDL.Texture
loadTexture renderer path = Image.imgLoadTexture renderer path >>= either throwSDLError return

getTextureSize :: SDL.Texture -> IO (CInt, CInt)
getTextureSize tex = alloca2 $ \w h -> do
    _ <- SDL.queryTexture tex nullPtr nullPtr w h
    peek2 (w, h)

createTextureFromSurface :: SDL.Renderer -> Ptr SDL.Surface -> IO (Either String SDL.Texture)
createTextureFromSurface renderer surface = do
    result <- SDL.createTextureFromSurface renderer surface
    return $ if result == nullPtr then Left "Unable to create texture" else Right result
