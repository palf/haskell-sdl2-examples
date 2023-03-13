{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)


draw :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
draw r t = do
  SDL.clear r
  SDL.copy r t Nothing Nothing
  SDL.present r


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Lesson 07" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      t <- SDL.Image.loadTexture r "./assets/texture.png"

      draw r t
      whileM $ not . C.hasQuitEvent <$> SDL.pollEvents

      SDL.destroyTexture t
