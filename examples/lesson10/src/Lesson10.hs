{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           SDL                    (($=))


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


loadTextures :: (MonadIO m) => SDL.Renderer -> PathMap -> m TextureMap
loadTextures r = mapM (C.loadTextureWithInfo r)


renderTexture
  :: (Num a, RealFrac a, MonadIO m)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> (a, a)
  -> m ()

renderTexture r (t, ti) (x, y)
  = SDL.copy r t Nothing (Just $ C.mkRect x' y' a b)

  where
    x' = floor x
    y' = floor y
    a = SDL.textureWidth ti
    b = SDL.textureHeight ti


draw :: (MonadIO m) => SDL.Renderer -> TextureMap -> m ()
draw r ts = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r

  renderTexture r (background ts) (0, 0 :: Double)
  renderTexture r (foreground ts) (240, 190 :: Double)

  SDL.present r


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 10" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      ts <- loadTextures r assetPaths
      draw r ts

      whileM $ not . C.hasQuitEvent <$> SDL.pollEvents

      mapM_ (SDL.destroyTexture . fst) ts
