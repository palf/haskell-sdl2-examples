{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)


main :: IO ()
main = withSDL $ withWindow "Lesson 01" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    -- pixelFormat <- SDL.surfaceFormat `applyToPointer` screen
    -- color <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF
    SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
    SDL.updateWindowSurface w

    SDL.delay 2000

    SDL.freeSurface screen


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit


withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (w, h) op = do
  window <- SDL.createWindow title windowProps
  SDL.showWindow window
  void $ op window
  SDL.destroyWindow window

    where
      windowProps = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral w) (fromIntegral h)
