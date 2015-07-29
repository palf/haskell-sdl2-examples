module Main (main) where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types
import Shared.Textures
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities


title :: String
title = "lesson09"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

main :: IO ()
main = inWindow $ withRenderer $ \renderer -> do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    texture <- loadTexture renderer "./assets/viewport.png"
    repeatUntilTrue $ draw renderer texture >> pollForQuit pollEvent
    SDL.destroyTexture texture

draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw renderer texture = do
    _ <- SDL.renderClear renderer
    _ <- inViewport topLeft >> drawTexture
    _ <- inViewport topRight >> drawTexture
    _ <- inViewport bottom >> drawTexture
    SDL.renderPresent renderer

    where inViewport rect = with rect $ SDL.renderSetViewport renderer
          drawTexture = SDL.renderCopy renderer texture nullPtr nullPtr
          topLeft = SDL.Rect { rectX = 0, rectY = 0, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          topRight = SDL.Rect { rectX = screenWidth `div` 2, rectY = 0, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          bottom = SDL.Rect { rectX = 0, rectY = screenHeight `div` 2, rectW = screenWidth, rectH = screenHeight `div` 2 }
          screenWidth = fst size
          screenHeight = snd size

