module Main (main) where

import Foreign.C.Types
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson08"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

fullWindow :: SDL.Rect
fullWindow = SDL.Rect {
    rectX = 0,
    rectY = 0,
    rectW = fst size,
    rectH = snd size }

main :: IO ()
main = inWindow (withRenderer doRender)

doRender :: SDL.Renderer -> IO ()
doRender renderer = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    repeatUntilTrue $ drawAll renderer >> handleNoInput pollEvent

data Colour = White | Red | Blue | Green | Yellow

draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw renderer texture = do
    _ <- SDL.renderClear renderer
    _ <- SDL.renderCopy renderer texture nullPtr nullPtr
    SDL.renderPresent renderer

drawAll :: SDL.Renderer -> IO ()
drawAll renderer = do
    _ <- clearScreen renderer
    _ <- withColor Red >> fillRectangle' innerRect
    _ <- withColor Green >> drawRectangle' outerRect
    _ <- withColor Blue >> drawLine' (0, screenHeight `div` 2) (screenWidth, screenHeight `div` 2)
    _ <- withColor Yellow >> mapM_ (\y -> drawDot' (screenWidth `div` 2, y)) [ 0, 4 .. screenHeight ]
    SDL.renderPresent renderer

    where innerRect = SDL.Rect { rectX = screenWidth `div` 4, rectY = screenHeight `div` 4, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          outerRect = SDL.Rect { rectX = screenWidth `div` 6, rectY = screenHeight `div` 6, rectW = 2 * screenWidth `div` 3, rectH = 2 * screenHeight `div` 3 }
          withColor = setColor renderer
          fillRectangle' = fillRectangle renderer
          drawRectangle' = drawRectangle renderer
          drawLine' = drawLine renderer
          drawDot' = drawDot renderer
          screenWidth = fst size
          screenHeight = snd size

clearScreen :: SDL.Renderer -> IO CInt
clearScreen renderer = do
    _ <- setColor renderer White
    SDL.renderClear renderer

fillRectangle :: SDL.Renderer -> SDL.Rect -> IO CInt
fillRectangle renderer shape = with shape $ SDL.renderFillRect renderer

drawRectangle :: SDL.Renderer -> SDL.Rect -> IO CInt
drawRectangle renderer shape = with shape $ SDL.renderDrawRect renderer

drawLine :: SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> IO CInt
drawLine renderer (ox, oy) (tx, ty) = SDL.renderDrawLine renderer ox oy tx ty

drawDot :: SDL.Renderer -> (CInt, CInt) -> IO CInt
drawDot renderer (x, y) = SDL.renderDrawPoint renderer x y

setColor :: SDL.Renderer -> Colour -> IO CInt
setColor renderer White  = SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColor renderer Red    = SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF
setColor renderer Green  = SDL.setRenderDrawColor renderer 0x00 0xFF 0x00 0xFF
setColor renderer Blue   = SDL.setRenderDrawColor renderer 0x00 0x00 0xFF 0xFF
setColor renderer Yellow = SDL.setRenderDrawColor renderer 0xFF 0xFF 0x00 0xFF

