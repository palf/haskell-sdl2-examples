module Main where

import qualified Graphics.UI.SDL as SDL
import Control.Monad
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Word


type Risky a = Either String a


lessonTitle :: String
lessonTitle = "lesson01"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480


main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo] >>= either throwSDLError return
    window <- createWindow lessonTitle >>= either throwSDLError return

    screenSurface <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screenSurface
    color <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF

    SDL.fillRect screenSurface nullPtr color
    SDL.updateWindowSurface window
    SDL.delay 2000

    SDL.destroyWindow window
    SDL.quit


initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize!" else Right ()


createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined screenWidth screenHeight SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer
