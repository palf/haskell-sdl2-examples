module Main where

import qualified Graphics.UI.SDL as SDL
import Shared.Assets
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson03"

size :: ScreenSize
size = (640, 480)

main :: IO ()
main = withSDL $ withWindow title size $ \window -> do
    screenSurface <- SDL.getWindowSurface window
    imageSurface <- loadBitmap "./assets/x.bmp" >>= either throwSDLError return
    repeatUntil sdlQuit $ SDL.blitSurface imageSurface nullPtr screenSurface nullPtr >> SDL.updateWindowSurface window
    SDL.freeSurface imageSurface

