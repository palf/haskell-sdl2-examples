{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra    (whileM)
import Data.Text              (Text)

main :: IO ()
main = withSDL $ withWindow "Lesson 03" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    image <- SDL.loadBMP "./assets/x.bmp"

    let doRender = renderSurfaceToWindow w screen image

    whileM $ SDL.pollEvent >>= f doRender

    SDL.freeSurface image
    SDL.freeSurface screen


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize [SDL.InitVideo]
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


f :: (Monad m) => m a -> Maybe SDL.Event -> m Bool
f p event
  = case event of
      Just (SDL.Event _t SDL.QuitEvent) ->
        pure False

      _ ->
        const True <$> p
