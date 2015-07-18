{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson04"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

drawInWindow :: forall a. (RenderOperation -> IO a) -> IO ()
drawInWindow drawFunc = inWindow $ \window -> do
    screenSurface <- SDL.getWindowSurface window
    drawFunc $ drawOn window screenSurface
    SDL.freeSurface screenSurface

surfacePaths :: [FilePath]
surfacePaths = [
    "./assets/press.bmp" ,
    "./assets/up.bmp" ,
    "./assets/down.bmp" ,
    "./assets/left.bmp" ,
    "./assets/right.bmp" ]

assetMap :: forall a. [a] -> KeyDirection -> a
assetMap surfaces KeyUp = surfaces !! 1
assetMap surfaces KeyDown = surfaces !! 2
assetMap surfaces KeyLeft = surfaces !! 3
assetMap surfaces KeyRight = surfaces !! 4
assetMap surfaces _ = head surfaces

main :: IO ()
main = drawInWindow $ \draw -> do
    surfaces <- mapM getSurfaceFrom surfacePaths
    let drawAsset = draw . assetMap surfaces
    draw (head surfaces)
    repeatUntilTrue $ handleKeyInput pollEvent drawAsset
    mapM_ SDL.freeSurface surfaces

