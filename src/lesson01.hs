module Main (main) where

import qualified Graphics.UI.SDL as SDL
import Shared.Lifecycle
import Shared.Utils


title :: String
title = "lesson01"

size :: ScreenSize
size = (640, 480)

main :: IO ()
main = withSDL $ withWindow title size $ \window -> do
    screenSurface <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screenSurface
    color <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF
    _ <- SDL.fillRect screenSurface nullPtr color
    _ <- SDL.updateWindowSurface window
    SDL.delay 2000
    SDL.freeSurface screenSurface

