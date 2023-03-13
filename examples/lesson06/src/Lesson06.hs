{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)


draw :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
draw w s t
  = SDL.surfaceBlitScaled t Nothing s Nothing
  >> SDL.updateWindowSurface w


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 06" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    pixelFormat <- SDL.surfaceFormat screen

    image <- SDL.Image.load "./assets/loaded.png"
    surface <- SDL.convertSurface image pixelFormat

    draw w screen surface
    whileM $ not . C.hasQuitEvent <$> SDL.pollEvents

    SDL.freeSurface image
    SDL.freeSurface surface
    SDL.freeSurface screen
