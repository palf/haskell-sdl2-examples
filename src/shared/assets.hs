module Shared.Assets where

import qualified Graphics.UI.SDL as SDL
import Foreign.C.String
import Foreign.Ptr

loadBitmap :: String -> IO (Either String (Ptr SDL.Surface))
loadBitmap path = do
    surface <- withCAString path SDL.loadBMP
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface

