module Shared.Drawing (
    withBlankScreen
) where

import qualified Graphics.UI.SDL as SDL
import Control.Monad


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    clearScreen renderer
    _ <- operation
    drawToScreen renderer

clearScreen :: SDL.Renderer -> IO ()
clearScreen renderer = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    return ()

drawToScreen :: SDL.Renderer -> IO ()
drawToScreen renderer = void $ SDL.renderPresent renderer

