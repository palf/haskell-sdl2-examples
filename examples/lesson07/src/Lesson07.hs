{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Lesson 07" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      t <- SDL.Image.loadTexture r "./assets/texture.png"

      whileM $ do
        ev <- SDL.pollEvents
        if C.hasQuitEvent ev
          then pure False
          else draw r t >> pure True

      SDL.destroyTexture t


draw :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
draw r t = do
  SDL.clear r
  SDL.copy r t Nothing Nothing
  SDL.present r
