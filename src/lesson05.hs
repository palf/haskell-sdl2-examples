module Main (main) where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson05"

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
    screenSurface <- SDL.getWindowSurface window
    imageSurface <- getSurfaceFrom "./assets/stretch.bmp"
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screenSurface
    stretchedSurface <- optimizeSurface imageSurface pixelFormat 0 >>= either throwSDLError return
    let draw surface stretch = with stretch (SDL.blitScaled surface nullPtr screenSurface) >> SDL.updateWindowSurface window
    let stretchRect = fullWindow
    repeatUntilTrue $ draw stretchedSurface stretchRect >> handleNoInput pollEvent
    SDL.freeSurface imageSurface
    SDL.freeSurface stretchedSurface
    SDL.freeSurface screenSurface

