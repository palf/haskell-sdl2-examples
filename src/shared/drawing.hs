module Shared.Drawing where

import qualified Graphics.UI.SDL as SDL


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- operation
    _ <- SDL.renderPresent renderer
    return ()
