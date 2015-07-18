module Main where

import Foreign.C.Types
import qualified Graphics.UI.SDL as SDL
--import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson09"

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
main = inWindow $ \window -> do
    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    texture <- loadTexture renderer "./assets/viewport.png" >>= either throwSDLError return
    repeatUntilTrue $ draw renderer texture >> handleNoInput pollEvent
    SDL.destroyTexture texture
    SDL.destroyRenderer renderer


draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw renderer texture = do
    SDL.renderClear renderer
    inViewport topLeft >> drawTexture
    inViewport topRight >> drawTexture
    inViewport bottom >> drawTexture
    SDL.renderPresent renderer

    where inViewport rect = with rect $ SDL.renderSetViewport renderer
          drawTexture = SDL.renderCopy renderer texture nullPtr nullPtr
          topLeft = SDL.Rect { rectX = 0, rectY = 0, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          topRight = SDL.Rect { rectX = screenWidth `div` 2, rectY = 0, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          bottom = SDL.Rect { rectX = 0, rectY = screenHeight `div` 2, rectW = screenWidth, rectH = screenHeight `div` 2 }
          screenWidth = fst size
          screenHeight = snd size

