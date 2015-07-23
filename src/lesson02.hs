module Main (main) where

import qualified Graphics.UI.SDL as SDL
import Shared.Assets
import Shared.Lifecycle
import Shared.Utilities


title :: String
title = "lesson02"

size :: ScreenSize
size = (640, 480)

main :: IO ()
main = withSDL $ withWindow title size $ \window -> do
    screenSurface <- SDL.getWindowSurface window
    imageSurface <- loadBitmap "./assets/hello_world.bmp" >>= either throwSDLError return
    _ <- SDL.blitSurface imageSurface nullPtr screenSurface nullPtr
    _ <- SDL.updateWindowSurface window
    SDL.delay 2000
    SDL.freeSurface imageSurface

