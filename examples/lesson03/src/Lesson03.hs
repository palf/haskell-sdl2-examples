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
      isContinue <$> SDL.pollEvent
      >>= conditionallyRun doRender

    SDL.freeSurface image
    SDL.freeSurface screen


isContinue :: Maybe SDL.Event -> Bool
isContinue Nothing = True
isContinue (Just e) = not $ C.isQuitEvent e


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = True <$ f
conditionallyRun _ False = pure False
