{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.IO.Class (MonadIO)
import SDL                    (($=))
import Foreign.C.Types        (CInt)
import Control.Monad.Extra (whileM)


data Colour = White | Red | Blue | Green | Yellow


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Lesson 08" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      setColor r White

      whileM $
        isContinue <$> SDL.pollEvent
        >>= conditionallyRun (draw r)


draw :: SDL.Renderer -> IO ()
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
      screenWidth = 640
      screenHeight = 480


mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
  where o = SDL.P (SDL.V2 x y)
        s = SDL.V2 w h


clearScreen :: (MonadIO m) => SDL.Renderer -> m ()
clearScreen r = do
    setColor r White
    SDL.clear r


fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


drawRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
drawRectangle r s = SDL.drawRect r (Just s)


drawLine :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine r (ox, oy) (tx, ty) =
  SDL.drawLine r (SDL.P (SDL.V2 ox oy)) (SDL.P (SDL.V2 tx ty))


drawDot :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> m ()
drawDot r (x, y) = SDL.drawPoint r (SDL.P (SDL.V2 x y))


setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound


isContinue :: Maybe SDL.Event -> Bool
isContinue Nothing = True
isContinue (Just e) = not $ C.isQuitEvent e


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = const True <$> f
conditionallyRun _ False = pure False
