{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson06"

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
    initializeSDLImage [Image.InitPNG] >>= catchRisky
    screenSurface <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screenSurface
    loadedSurface <- getSurfaceFrom "./assets/loaded.png"
    imageSurface <- convertSurface loadedSurface pixelFormat 0 >>= catchRisky
    let draw surface = SDL.blitScaled surface nullPtr screenSurface nullPtr >> SDL.updateWindowSurface window
    repeatUntilComplete $ draw imageSurface >> handleSimple pollEvent
    SDL.freeSurface loadedSurface
    SDL.freeSurface imageSurface
    Image.quit

catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return

initializeSDLImage :: [Image.InitFlag] -> IO (Risky CInt)
initializeSDLImage flags = do
    initSuccess <- Image.init $ Image.initFlagsToC flags
    return $ if initSuccess < 0 then Left "SDL_image could not initialize!" else Right initSuccess

