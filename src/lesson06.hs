module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Shared.Assets
import Shared.Image
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities

title :: String
title = "lesson06"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    screenSurface <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screenSurface
    loadedSurface <- getSurfaceFrom' "./assets/loaded.png"
    imageSurface <- optimizeSurface loadedSurface pixelFormat 0 >>= either throwSDLError return
    _ <- SDL.freeSurface loadedSurface
    let draw surface = SDL.blitScaled surface nullPtr screenSurface nullPtr >> SDL.updateWindowSurface window
    repeatUntilTrue $ draw imageSurface >> handleNoInput pollEvent
    SDL.freeSurface imageSurface

