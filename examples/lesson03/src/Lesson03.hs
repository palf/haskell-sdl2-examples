{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.Extra    (whileM)


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 03" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    image <- SDL.loadBMP "./assets/x.bmp"

    let doRender = C.renderSurfaceToWindow w screen image

    whileM $
      C.isContinue <$> SDL.pollEvent
      >>= C.conditionallyRun doRender

    SDL.freeSurface image
    SDL.freeSurface screen
