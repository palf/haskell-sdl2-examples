module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word


type Risky a = Either String a


lessonTitle :: String
lessonTitle = "lesson06"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

fullWindow :: SDL.Rect
fullWindow = SDL.Rect {
    rectX = 0,
    rectY = 0,
    rectW = screenWidth,
    rectH = screenHeight }


main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    window <- createWindow lessonTitle >>= catchRisky
    initializeSDLImage [Image.InitPNG] >>= catchRisky

    screenSurface <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat `applyToPointer` screenSurface

    loadedSurface <- loadSurface "./assets/loaded.png" >>= catchRisky
    imageSurface <- convertSurface loadedSurface pixelFormat 0 >>= catchRisky

    let draw surface = SDL.blitScaled surface nullPtr screenSurface nullPtr >> SDL.updateWindowSurface window

    repeatUntilComplete $ draw imageSurface >> handle pollEvent

    SDL.freeSurface loadedSurface
    SDL.freeSurface imageSurface
    SDL.destroyWindow window
    SDL.quit
    Image.quit


catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return


initializeSDL :: [Word32] -> IO (Risky CInt)
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize!" else Right initSuccess


initializeSDLImage :: [Image.InitFlag] -> IO (Risky CInt)
initializeSDLImage flags = do
    initSuccess <- Image.init $ Image.initFlagsToC flags
    return $ if initSuccess < 0 then Left "SDL_image could not initialize!" else Right initSuccess


createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined screenWidth screenHeight SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


loadSurface :: String -> IO (Risky (Ptr SDL.Surface))
loadSurface path = do
    surface <- withCAString path Image.load
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface


convertSurface :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Risky (Ptr SDL.Surface))
convertSurface surface format flags = do
    optimizedSurface <- SDL.convertSurface surface format flags
    return $ if optimizedSurface == nullPtr then Left "Unable to optimize image!" else Right optimizedSurface


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


repeatUntilComplete :: IO Bool -> IO ()
repeatUntilComplete operation = do
    complete <- operation
    unless complete $ repeatUntilComplete operation


handle :: IO (Maybe SDL.Event) -> IO Bool
handle stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False

        Just (SDL.QuitEvent _ _) -> return True

        _ -> return False


pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pointer -> do
    status <- SDL.pollEvent pointer

    if status == 1
        then maybePeek peek pointer
        else return Nothing


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer
