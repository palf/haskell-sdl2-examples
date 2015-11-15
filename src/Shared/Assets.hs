module Shared.Assets (
    withAssets,
    Asset
) where

import qualified Graphics.UI.SDL as SDL
import Foreign.C.Types
import Shared.Textures

type Asset = (SDL.Texture, CInt, CInt)

withAssets :: SDL.Renderer -> [FilePath] -> ([Asset] -> IO a) -> IO ()
withAssets renderer paths f = do
    assets <- mapM (loadAsset renderer) paths
    _ <- f assets
    mapM_ destroyAsset assets

loadAsset :: SDL.Renderer -> FilePath -> IO Asset
loadAsset renderer path = do
    texture <- loadTexture renderer path
    (w, h) <- getTextureSize texture
    return (texture, w, h)

destroyAsset :: Asset -> IO ()
destroyAsset (texture, _, _) = SDL.destroyTexture texture
