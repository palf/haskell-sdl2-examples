{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.Extra    (whileM)


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 05" (640, 480) $
  \window -> do

    screen <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat screen

    image <- SDL.loadBMP "./assets/stretch.bmp"
    surface <- SDL.convertSurface image pixelFormat

    let draw =
          SDL.surfaceBlitScaled surface Nothing screen Nothing >>
            SDL.updateWindowSurface window

    whileM $
      isContinue <$> SDL.pollEvent
      >>= conditionallyRun draw

    SDL.freeSurface image
    SDL.freeSurface surface
    SDL.freeSurface screen


isContinue :: Maybe SDL.Event -> Bool
isContinue Nothing = True
isContinue (Just e) = not $ C.isQuitEvent e


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = const True <$> f
conditionallyRun _ False = pure False
