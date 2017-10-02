{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image
import qualified Common as C

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (whileM)
import SDL                 (($=))

data AssetMap a = AssetMap
  { background :: a
  , foreground :: a
  } deriving (Functor, Foldable, Traversable)


type PathMap = AssetMap FilePath
type TextureMap = AssetMap (SDL.Texture, SDL.TextureInfo)


assetPaths :: PathMap
assetPaths = AssetMap
  { background = "./assets/background.png"
  , foreground = "./assets/foo.png"
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 10" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      ts <- loadTextures r assetPaths
      let doRender = draw r ts

      whileM $
        C.isContinue <$> SDL.pollEvent
        >>= C.conditionallyRun doRender

      mapM_ (SDL.destroyTexture . fst) ts


loadTextures :: (MonadIO m) => SDL.Renderer -> PathMap -> m TextureMap
loadTextures r = mapM (loadTexture r)


loadTexture :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTexture r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


draw :: SDL.Renderer -> TextureMap -> IO ()
draw r ts = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r

  renderTexture r (background ts) (0, 0 :: Double)
  renderTexture r (foreground ts) (240, 190 :: Double)

  SDL.present r


renderTexture
  :: (Num a, RealFrac a)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> (a, a)
  -> IO ()

renderTexture r (t, ti) (x, y)
  = SDL.copy r t
      Nothing
      (Just $ C.mkRect x' y' a b)

  where
    x' = floor x
    y' = floor y
    a = SDL.textureWidth ti
    b = SDL.textureHeight ti
