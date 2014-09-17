module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
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
lessonTitle = "lesson12"

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

initialState :: World
initialState = World { gameover = False, red = 128, green = 128, blue = 128 }


---- Application ----

main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    initializeSDLImage [Image.InitPNG] >>= catchRisky

    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    window <- createWindow lessonTitle >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated] >>= catchRisky

    asset@(texture, _, _) <- loadTexture renderer "./assets/colors.png" >>= catchRisky

    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~> drawState renderer asset
    runStateT (repeatUntilComplete pollDraw) initialState

    destroyTextures [texture]
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    Image.quit


data Colour = Red | Green | Blue
data Key = Q | W | E | A | S | D | N
data World = World { gameover :: Bool, red :: Word8, green :: Word8, blue :: Word8 }
type Input = Maybe SDL.Event
type Asset = (SDL.Texture, CInt, CInt)




drawState :: SDL.Renderer -> Asset -> World -> IO World
drawState renderer (texture, width, height) state@(World False r g b) = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    SDL.setTextureColorMod texture r g b
    renderTexture' position
    SDL.renderPresent renderer
    return state
    where renderTexture' renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr
          position = SDL.Rect { rectX = 0, rectY = 0, rectW = width, rectH = height }
drawState _ _ state = return state


updateState :: Input -> World -> World
updateState (Just (SDL.KeyboardEvent evtType _ _ _ _ keysym)) state = if evtType == SDL.eventTypeKeyDown then modifyState state keysym else state
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state


modifyState :: World -> SDL.Keysym -> World
modifyState state keysym = case getKey keysym of
    Q -> state `increase` Red
    W -> state `increase` Green
    E -> state `increase` Blue
    A -> state `decrease` Red
    S -> state `decrease` Green
    D -> state `decrease` Blue
    _ -> state


repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = do
    state <- game
    unless (gameover state) $ repeatUntilComplete game


increase :: World -> Colour -> World
increase state Red = state { red = red state + 16 }
increase state Green = state { green = green state + 16 }
increase state Blue = state { blue = blue state + 16 }


decrease :: World -> Colour -> World
decrease state Red = state { red = red state - 16 }
decrease state Green = state { green = green state - 16 }
decrease state Blue = state { blue = blue state - 16 }


getKey :: SDL.Keysym -> Key
getKey keysym = case keysymScancode keysym of
    20 -> Q
    26 -> W
    8  -> E
    4  -> A
    22 -> S
    7  -> D
    _  -> N

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


---- Teardown ----

destroyTextures :: [SDL.Texture] -> IO ()
destroyTextures = mapM_ SDL.destroyTexture


---- Surfacing & Texture Loading ----

loadSurface :: String -> IO (Risky (Ptr SDL.Surface))
loadSurface path = do
    surface <- withCAString path Image.load
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface


convertSurface :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Risky (Ptr SDL.Surface))
convertSurface surface format flags = do
    optimizedSurface <- SDL.convertSurface surface format flags
    return $ if optimizedSurface == nullPtr then Left "Unable to optimize image!" else Right optimizedSurface


loadTexture :: SDL.Renderer -> String -> IO (Risky Asset)
loadTexture renderer path = do
    loadedSurface <- loadSurface path >>= catchRisky
    let applyToSurface = flip applyToPointer loadedSurface

    width <- applyToSurface SDL.surfaceW
    height <- applyToSurface SDL.surfaceH
    pixelFormat <- applyToSurface SDL.surfaceFormat
    key <- SDL.mapRGB pixelFormat 0 0xFF 0xFF

    SDL.setColorKey loadedSurface 1 key
    newTexture <- createTextureFromSurface renderer loadedSurface >>= catchRisky

    SDL.freeSurface loadedSurface
    return $ if newTexture == nullPtr then Left "failed to load texture image" else Right (newTexture, width, height)


createTextureFromSurface :: SDL.Renderer -> Ptr SDL.Surface -> IO (Risky Texture)
createTextureFromSurface renderer surface = do
    result <- SDL.createTextureFromSurface renderer surface
    return $ if result == nullPtr then Left "Unable to create texture" else Right result


renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> SDL.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.renderCopy renderer texture


---- Geometry ----

moveTo :: SDL.Rect -> (CInt, CInt) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = x, rectY = y }


---- Event Handling ----

pollEvent :: IO Input
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

with2 :: (Storable a, Storable b) => a -> b -> (Ptr a -> Ptr b -> IO c) -> IO c
with2 a b f = with a $ \a' -> with b (f a')


withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b f = withCAString a $ \a' -> withCAString b $ f a'


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer


infixl 4 ~>
(~>) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m b
(~>) m f = m >>= lift . f


into :: (Monad m, MonadTrans t, MonadState b (t m)) => m a -> (a -> b -> b) -> t m b
into source f = lift source >>= modify . f >> get
