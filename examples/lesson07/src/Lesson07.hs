{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image
import qualified Common as C

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (whileM)


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Lesson 07" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      tex <- SDL.Image.loadTexture r "./assets/texture.png"

      whileM $
        isContinue <$> SDL.pollEvent
        >>= conditionallyRun (draw r tex)

      SDL.destroyTexture tex


draw :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
draw r t = do
  SDL.clear r
  SDL.copy r t Nothing Nothing
  SDL.present r


isContinue :: Maybe SDL.Event -> Bool
isContinue Nothing = True
isContinue (Just e) = not $ C.isQuitEvent e


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = const True <$> f
conditionallyRun _ False = pure False
