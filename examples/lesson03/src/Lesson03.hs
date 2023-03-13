{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.IO.Class (MonadIO)


withBitmap :: (MonadIO m) => FilePath -> (SDL.Surface -> m a) -> m ()
withBitmap path op =  do
  image <- SDL.loadBMP path
  _ <- op image
  SDL.freeSurface image


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 03" (640, 480) $
  \w -> do
    screen <- SDL.getWindowSurface w
    withBitmap "./assets/x.bmp" $ \image -> untilQuit $ appLoop $ do
      _ <- SDL.surfaceBlit image Nothing screen Nothing
      SDL.updateWindowSurface w

    SDL.freeSurface screen



data Status = Quit | Idle


appLoop :: (MonadIO m) => m () -> m Status
appLoop doRender = do
  ev <- SDL.pollEvents
  if C.hasQuitEvent ev
    then pure Quit
    else doRender >> pure Idle


untilQuit :: (Monad m) => m Status -> m ()
untilQuit op = loop
  where
    loop = op >>= \case Idle -> loop
                        Quit -> pure ()
