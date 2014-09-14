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


---- Config ----

lessonTitle :: String
lessonTitle = "lesson09"

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


---- Application ----

main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    initializeSDLImage [Image.InitPNG] >>= catchRisky

    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    window <- createWindow lessonTitle >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated] >>= catchRisky
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF

    texture <- loadTexture renderer "./assets/viewport.png" >>= catchRisky
    repeatUntilComplete $ draw renderer texture >> handle pollEvent

    SDL.destroyTexture texture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    Image.quit


---- Initialization ----

initializeSDL :: [Word32] -> IO (Risky CInt)
initializeSDL flags = do
    result <- SDL.init $ foldl (.|.) 0 flags
    return $ if result < 0 then Left "SDL could not initialize!" else Right result


initializeSDLImage :: [Image.InitFlag] -> IO (Risky CInt)
initializeSDLImage flags = do
    result <- Image.init $ Image.initFlagsToC flags
    return $ if result < 0 then Left "SDL_image could not initialize!" else Right result


createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined screenWidth screenHeight SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


createRenderer :: SDL.Window -> CInt -> [Word32] -> IO (Risky SDL.Renderer)
createRenderer window index flags = do
    renderer <- SDL.createRenderer window index $ foldl (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created!" else Right renderer


setHint :: String -> String -> IO (Risky Bool)
setHint hint value = do
    result <- withCAString2 hint value SDL.setHint
    return $ if not result then Left "Warning: Linear texture filtering not enabled!" else Right result


---- Surfacing & Texture Loading ----

loadSurface :: String -> IO (Risky (Ptr SDL.Surface))
loadSurface path = do
    surface <- withCAString path Image.load
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface


convertSurface :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Risky (Ptr SDL.Surface))
convertSurface surface format flags = do
    optimizedSurface <- SDL.convertSurface surface format flags
    return $ if optimizedSurface == nullPtr then Left "Unable to optimize image!" else Right optimizedSurface


loadTexture :: SDL.Renderer -> String -> IO (Risky SDL.Texture)
loadTexture renderer path = do
    loadedSurface <- loadSurface path >>= catchRisky
    newTexture <- createTextureFromSurface renderer loadedSurface >>= catchRisky
    SDL.freeSurface loadedSurface
    return $ if newTexture == nullPtr then Left "failed to load texture image" else Right newTexture


createTextureFromSurface :: SDL.Renderer -> Ptr SDL.Surface -> IO (Risky Texture)
createTextureFromSurface renderer surface = do
    result <- SDL.createTextureFromSurface renderer surface
    return $ if result == nullPtr then Left "Unable to create texture" else Right result


draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw renderer texture = do
    SDL.renderClear renderer
    inViewport topLeft >> drawTexture
    inViewport topRight >> drawTexture
    inViewport bottom >> drawTexture
    SDL.renderPresent renderer

    where inViewport rect = with rect $ SDL.renderSetViewport renderer
          drawTexture = SDL.renderCopy renderer texture nullPtr nullPtr
          topLeft = SDL.Rect { rectX = 0, rectY = 0, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          topRight = SDL.Rect { rectX = screenWidth `div` 2, rectY = 0, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          bottom = SDL.Rect { rectX = 0, rectY = screenHeight `div` 2, rectW = screenWidth, rectH = screenHeight `div` 2 }


---- Event Handling ----

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


---- Error Handling ----

type Risky a = Either String a


catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return


logWarning :: Risky Bool -> IO Bool
logWarning = either (\x -> print x >> return False) return


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


---- Utils ----

withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b op = withCAString a $ \a' -> withCAString b $ op a'


repeatUntilComplete :: IO Bool -> IO ()
repeatUntilComplete operation = do
    complete <- operation
    unless complete $ repeatUntilComplete operation


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer
