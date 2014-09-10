module Main where

import qualified Graphics.UI.SDL as SDL
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
lessonTitle = "lesson04"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480


data KeyDirection = KeyUp | KeyDown | KeyLeft | KeyRight | KeyOther deriving (Show, Read)

main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo] >>= either throwSDLError return
    window <- createWindow lessonTitle >>= either throwSDLError return

    screenSurface <- SDL.getWindowSurface window

    upSurface <- loadBitmap "./assets/up.bmp" >>= either throwSDLError return
    downSurface <- loadBitmap "./assets/down.bmp" >>= either throwSDLError return
    leftSurface <- loadBitmap "./assets/left.bmp" >>= either throwSDLError return
    rightSurface <- loadBitmap "./assets/right.bmp" >>= either throwSDLError return
    defaultSurface <- loadBitmap "./assets/press.bmp" >>= either throwSDLError return

    let draw surface = SDL.blitSurface surface nullPtr screenSurface nullPtr >> SDL.updateWindowSurface window
    let drawMap KeyUp = draw upSurface
        drawMap KeyDown = draw downSurface
        drawMap KeyLeft = draw leftSurface
        drawMap KeyRight = draw rightSurface
        drawMap _ = draw defaultSurface

    draw defaultSurface
    repeatUntilComplete $ handle pollEvent drawMap

    mapM_ SDL.freeSurface [ defaultSurface, upSurface, downSurface, leftSurface, rightSurface ]
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


loadBitmap :: String -> IO (Risky (Ptr SDL.Surface))
loadBitmap path = do
    surface <- withCAString path SDL.loadBMP
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


repeatUntilComplete :: IO Bool -> IO ()
repeatUntilComplete operation = do
    complete <- operation
    unless complete $ repeatUntilComplete operation


handle :: IO (Maybe SDL.Event) -> (KeyDirection -> IO a) -> IO Bool
handle stream keyHandler = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False

        Just (SDL.QuitEvent _ _) -> return True

        Just (SDL.KeyboardEvent _ _ _ _ _ keysym) -> do
            keyHandler $ keymap keysym
            return False

        _ -> return False


keymap :: SDL.Keysym -> KeyDirection
keymap (SDL.Keysym keysymScancode _ _) = case keysymScancode of
    79 -> KeyRight
    80 -> KeyLeft
    81 -> KeyDown
    82 -> KeyUp
    _ -> KeyOther


pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pointer -> do
    status <- SDL.pollEvent pointer

    if status == 1
        then maybePeek peek pointer
        else return Nothing


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer
