{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Word
import           Foreign.C.Types        (CInt)
import           SDL                    (($=))


data Colour = White | Red | Blue | Green | Yellow


setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r c  = SDL.rendererDrawColor r $= getColor c
  where
    getColor :: Colour -> SDL.V4 Word8
    getColor White  = SDL.V4 maxBound maxBound maxBound maxBound
    getColor Red    = SDL.V4 maxBound 0 0 maxBound
    getColor Green  = SDL.V4 0 maxBound 0 maxBound
    getColor Blue   = SDL.V4 0 0 maxBound maxBound
    getColor Yellow = SDL.V4 maxBound maxBound 0 maxBound


clearScreen :: (MonadIO m) => SDL.Renderer -> m ()
clearScreen r = do
  setColor r White
  SDL.clear r


drawRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
drawRectangle r s = SDL.drawRect r (Just s)


fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


drawLine :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine r (ox, oy) (tx, ty) =
  SDL.drawLine r (C.mkPoint ox oy) (C.mkPoint tx ty)


drawDot :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> m ()
drawDot r (x, y) = SDL.drawPoint r (SDL.P (SDL.V2 x y))


mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
  where o = SDL.P (SDL.V2 x y)
        s = SDL.V2 w h



screenWidth :: (Num a) => a
screenWidth = 640


screenHeight :: (Num a) => a
screenHeight = 480


draw :: (MonadIO m) => SDL.Renderer -> m ()
draw r = do
  clearScreen r
  withColor Red    >> fillRectangle' innerRect
  withColor Green  >> drawRectangle' outerRect
  withColor Blue   >> drawLine' (0, screenHeight `div` 2) (screenWidth, screenHeight `div` 2)
  withColor Yellow >> mapM_ (\y -> drawDot' (screenWidth `div` 2, y)) [ 0, 4 .. screenHeight ]
  SDL.present r

  where
    innerRect = mkRect (screenWidth `div` 4) (screenHeight `div` 4) (screenWidth `div` 2) (screenHeight `div` 2)
    outerRect = mkRect (screenWidth `div` 6) (screenHeight `div` 6) (2 * screenWidth `div` 3) (2 * screenHeight `div` 3)
    withColor = setColor r
    fillRectangle' = fillRectangle r
    drawRectangle' = drawRectangle r
    drawLine' = drawLine r
    drawDot' = drawDot r


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Lesson 08" (screenWidth, screenHeight) $ \w ->
    C.withRenderer w $ \r -> do

      setColor r White

      whileM $ do
        ev <- SDL.pollEvents
        if C.hasQuitEvent ev
          then pure False
          else draw r >> pure True
