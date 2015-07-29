{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Graphics.UI.SDL as SDL
import Shared.DrawingSimple
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities


title :: String
title = "lesson04"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

drawInWindow :: forall a. (RenderOperation -> IO a) -> IO ()
drawInWindow drawFunc = inWindow $ \window -> do
    screenSurface <- SDL.getWindowSurface window
    _ <- drawFunc $ drawOn window screenSurface
    _ <- SDL.freeSurface screenSurface
    return ()

surfacePaths :: [FilePath]
surfacePaths = [
    "./assets/press.bmp" ,
    "./assets/up.bmp" ,
    "./assets/down.bmp" ,
    "./assets/left.bmp" ,
    "./assets/right.bmp" ]

selectSurface :: forall a. [a] -> KeyPress -> a
selectSurface surfaces KeyUp = surfaces !! 1
selectSurface surfaces KeyDown = surfaces !! 2
selectSurface surfaces KeyLeft = surfaces !! 3
selectSurface surfaces KeyRight = surfaces !! 4
selectSurface surfaces _ = head surfaces

handleKeyInput :: IO (Maybe SDL.Event) -> (KeyPress -> IO a) -> IO Bool
handleKeyInput stream keyHandler = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False
        Just (SDL.QuitEvent _ _) -> return True
        Just (SDL.KeyboardEvent _ _ _ _ _ keysym) -> do
            _ <- keyHandler $ getKey keysym
            return False
        _ -> return False

main :: IO ()
main = drawInWindow $ \draw -> do
    surfaces <- mapM getSurfaceFrom surfacePaths
    let drawSurfaceForKey = draw . selectSurface surfaces
    _ <- draw (head surfaces)
    repeatUntilTrue $ handleKeyInput pollEvent drawSurfaceForKey
    mapM_ SDL.freeSurface surfaces

