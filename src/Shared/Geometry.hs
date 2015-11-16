module Shared.Geometry where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types
import Foreign.C.Types

toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect { rectX = fromIntegral x, rectY = fromIntegral y, rectW = fromIntegral w, rectH = fromIntegral h }

moveBy :: (Integral a) => SDL.Rect -> (a, a) -> SDL.Rect
moveBy shape (dx, dy) = shape { rectX = rectX shape + fromIntegral dx, rectY = rectY shape + fromIntegral dy }

centredOn :: SDL.Rect -> SDL.Rect -> SDL.Rect
centredOn inner outer = inner `moveBy` (centreOf outer `pairMinus` centreOf inner)

centreOf :: SDL.Rect -> (CInt, CInt)
centreOf shape = (x, y)
    where x = rectX shape + rectW shape `div` 2
          y = rectY shape + rectH shape `div` 2

pairMinus :: (Num a) => (a, a) -> (a, a) -> (a, a)
pairMinus (a, b) (c, d) = (a - c, b - d)

toSDLPoint :: (CInt, CInt) -> SDL.Point
toSDLPoint (x, y) = SDL.Point { pointX = x, pointY = y }
