{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 02" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    image <- SDL.loadBMP "./assets/hello_world.bmp"

    C.renderSurfaceToWindow w screen image

    SDL.delay 2000
    SDL.freeSurface image
    SDL.freeSurface screen
