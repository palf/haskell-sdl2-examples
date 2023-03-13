{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 06" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    pixelFormat <- SDL.surfaceFormat screen

    image <- SDL.Image.load "./assets/loaded.png"
    surface <- SDL.convertSurface image pixelFormat

    whileM $ do
      ev <- SDL.pollEvents
      if C.hasQuitEvent ev
        then pure False
        else draw w screen surface >> pure True

    SDL.freeSurface image
    SDL.freeSurface surface
    SDL.freeSurface screen


draw :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
draw w s t
  = SDL.surfaceBlitScaled t Nothing s Nothing
  >> SDL.updateWindowSurface w

