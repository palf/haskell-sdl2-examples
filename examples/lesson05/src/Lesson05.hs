{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)


draw :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
draw w s t
  = SDL.surfaceBlitScaled t Nothing s Nothing
  >> SDL.updateWindowSurface w


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 05" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    pixelFormat <- SDL.surfaceFormat screen

    image <- SDL.loadBMP "./assets/stretch.bmp"
    surface <- SDL.convertSurface image pixelFormat

    draw w screen surface
    whileM $ not . C.hasQuitEvent <$> SDL.pollEvents

    mapM_ SDL.freeSurface [image, surface, screen]
