module Main where

import qualified Graphics.UI.SDL as SDL
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
lessonTitle = "lesson08"

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

    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    window <- createWindow lessonTitle >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated] >>= catchRisky
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF

    repeatUntilComplete $ drawAll renderer >> handle pollEvent

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit


---- Initialization ----

initializeSDL :: [Word32] -> IO (Risky CInt)
initializeSDL flags = do
    result <- SDL.init $ foldl (.|.) 0 flags
    return $ if result < 0 then Left "SDL could not initialize!" else Right result


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


---- Drawing ----

data Colour = White | Red | Blue | Green | Yellow


draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw renderer texture = do
    SDL.renderClear renderer
    SDL.renderCopy renderer texture nullPtr nullPtr
    SDL.renderPresent renderer


drawAll :: SDL.Renderer -> IO ()
drawAll renderer = do
    clearScreen renderer
    withColor Red >> fillRectangle' innerRect
    withColor Green >> drawRectangle' outerRect
    withColor Blue >> drawLine' (0, screenHeight `div` 2) (screenWidth, screenHeight `div` 2)
    withColor Yellow >> mapM_ (\y -> drawDot' (screenWidth `div` 2, y)) [ 0, 4 .. screenHeight ]
    SDL.renderPresent renderer

    where innerRect = SDL.Rect { rectX = screenWidth `div` 4, rectY = screenHeight `div` 4, rectW = screenWidth `div` 2, rectH = screenHeight `div` 2 }
          outerRect = SDL.Rect { rectX = screenWidth `div` 6, rectY = screenHeight `div` 6, rectW = 2 * screenWidth `div` 3, rectH = 2 * screenHeight `div` 3 }
          withColor = setColor renderer
          fillRectangle' = fillRectangle renderer
          drawRectangle' = drawRectangle renderer
          drawLine' = drawLine renderer
          drawDot' = drawDot renderer


clearScreen :: SDL.Renderer -> IO CInt
clearScreen renderer = do
    setColor renderer White
    SDL.renderClear renderer


fillRectangle :: SDL.Renderer -> SDL.Rect -> IO CInt
fillRectangle renderer shape = with shape $ SDL.renderFillRect renderer


drawRectangle :: SDL.Renderer -> SDL.Rect -> IO CInt
drawRectangle renderer shape = with shape $ SDL.renderDrawRect renderer


drawLine :: SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> IO CInt
drawLine renderer (ox, oy) (tx, ty) = SDL.renderDrawLine renderer ox oy tx ty


drawDot :: SDL.Renderer -> (CInt, CInt) -> IO CInt
drawDot renderer (x, y) = SDL.renderDrawPoint renderer x y


setColor :: SDL.Renderer -> Colour -> IO CInt
setColor renderer White  = SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColor renderer Red    = SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF
setColor renderer Green  = SDL.setRenderDrawColor renderer 0x00 0xFF 0x00 0xFF
setColor renderer Blue   = SDL.setRenderDrawColor renderer 0x00 0x00 0xFF 0xFF
setColor renderer Yellow = SDL.setRenderDrawColor renderer 0xFF 0xFF 0x00 0xFF


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
