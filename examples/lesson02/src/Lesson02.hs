{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)


main :: IO ()
main = withSDL $ withWindow "Lesson 02" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    image <- SDL.loadBMP "./assets/hello_world.bmp"

    renderSurfaceToWindow w screen image

    SDL.delay 2000
    SDL.freeSurface image
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


renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i
  = SDL.surfaceBlit i Nothing s Nothing
  >> SDL.updateWindowSurface w
