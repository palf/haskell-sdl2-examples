{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Foreign.C.Types        (CInt)
import           SDL                    (($=))


screenWidth :: CInt
screenWidth = 640


screenHeight :: CInt
screenHeight = 480


main :: IO ()
main = C.withSDL $
  C.withWindow "Lesson 09" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
      t <- SDL.Image.loadTexture r "./assets/viewport.png"

      let doRender = draw r t

      whileM $ do
        ev <- SDL.pollEvents
        if C.hasQuitEvent ev
          then pure False
          else doRender >> pure True

      SDL.destroyTexture t


draw :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
draw r t = do
  SDL.clear r

  setViewport r topLeft
  drawTexture r t

  setViewport r topRight
  drawTexture r t

  setViewport r bottom
  drawTexture r t

  SDL.present r

  where

    topLeft :: SDL.Rectangle CInt
    topLeft = C.mkRect 0 0 (screenWidth `div` 2) (screenHeight `div` 2)

    topRight :: SDL.Rectangle CInt
    topRight = C.mkRect (screenWidth `div` 2) 0 (screenWidth `div` 2) (screenHeight `div` 2)

    bottom :: SDL.Rectangle CInt
    bottom = C.mkRect 0 (screenHeight `div` 2) screenWidth (screenHeight `div` 2)


drawTexture :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
drawTexture r t = SDL.copy r t Nothing Nothing


setViewport :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
setViewport r s = SDL.rendererViewport r $= Just s
