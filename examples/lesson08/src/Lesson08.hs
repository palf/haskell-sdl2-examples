{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ask, runReaderT)
import           Data.Word
import           Foreign.C.Types        (CInt)
import           SDL                    (($=))


data Colour = White | Red | Blue | Green | Yellow


type HasRenderer m = ( MonadIO m, MonadReader SDL.Renderer m)


setColour :: (HasRenderer m) => Colour -> m ()
setColour c = do
  r <- ask
  SDL.rendererDrawColor r $= getColour c

  where
    getColour :: Colour -> SDL.V4 Word8
    getColour White  = SDL.V4 maxBound maxBound maxBound maxBound
    getColour Red    = SDL.V4 maxBound 0 0 maxBound
    getColour Green  = SDL.V4 0 maxBound 0 maxBound
    getColour Blue   = SDL.V4 0 0 maxBound maxBound
    getColour Yellow = SDL.V4 maxBound maxBound 0 maxBound


clearScreen :: (HasRenderer m) => m ()
clearScreen = do
  r <- ask
  setColour White
  SDL.clear r


drawRectangle :: (HasRenderer m) => SDL.Rectangle CInt -> m ()
drawRectangle s = ask >>= \r -> SDL.drawRect r (Just s)


fillRectangle :: (HasRenderer m) => SDL.Rectangle CInt -> m ()
fillRectangle s = ask >>= \r -> SDL.fillRect r (Just s)


drawLine :: (HasRenderer m) => (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine (ox, oy) (tx, ty) =
  ask >>= \r -> SDL.drawLine r (C.mkPoint ox oy) (C.mkPoint tx ty)


drawDot :: (HasRenderer m) => (CInt, CInt) -> m ()
drawDot (x, y) = ask >>= \r -> SDL.drawPoint r (SDL.P (SDL.V2 x y))


mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
  where o = SDL.P (SDL.V2 x y)
        s = SDL.V2 w h


screenWidth :: (Num a) => a
screenWidth = 640


screenHeight :: (Num a) => a
screenHeight = 480


draw :: (HasRenderer m) => m ()
draw = do
  clearScreen
  setColour Red    >> fillRectangle innerRect
  setColour Green  >> drawRectangle outerRect
  setColour Blue   >> drawLine (0, h `div` 2) (w, h `div` 2)
  setColour Yellow >> mapM_ (\y -> drawDot (w `div` 2, y)) [ 0, 4 .. h ]

  ask >>= SDL.present

  where
    w = screenWidth
    h = screenHeight
    innerRect = mkRect (w `div` 4) (h `div` 4) (w `div` 2) (h `div` 2)
    outerRect = mkRect (w `div` 6) (h `div` 6) (2 * w `div` 3) (2 * h `div` 3)


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Lesson 08" (screenWidth, screenHeight) $ \w ->
    C.withRenderer w $ \r -> do

      runReaderT (setColour White) r
      runReaderT draw r

      whileM $ not . C.hasQuitEvent <$> SDL.pollEvents
