{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.IO.Class (MonadIO)


data Status = Quit | Idle


withBitmap :: (MonadIO m) => FilePath -> (SDL.Surface -> m a) -> m ()
withBitmap path op =  do
  image <- SDL.loadBMP path
  _ <- op image
  SDL.freeSurface image


untilQuit :: (Monad m) => m Status -> m ()
untilQuit op = loop
  where
    loop = op >>= \case Idle -> loop
                        Quit -> pure ()


mapEventsToStatus :: [SDL.Event] -> Status
mapEventsToStatus evs =
  if C.hasQuitEvent evs
    then Quit
    else Idle


appLoop :: (MonadIO m) => m ()
appLoop = untilQuit $ mapEventsToStatus <$> SDL.pollEvents


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 03" (640, 480) $
  \w -> do
    screen <- SDL.getWindowSurface w

    let draw image = SDL.surfaceBlit image Nothing screen Nothing >> SDL.updateWindowSurface w

    withBitmap "./assets/x.bmp" $ \image -> do
      draw image
      appLoop

    SDL.freeSurface screen
