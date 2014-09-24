{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

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
lessonTitle = "lesson14"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

initialState :: World
initialState = World { gameover = False, frame = 0 }


---- Application ----

main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    initializeSDLImage [Image.InitPNG] >>= catchRisky

    setHint "SDL_RENDER_SCALE_QUALITY" "0" >>= logWarning
    window <- createWindow lessonTitle >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated, SDL.rendererFlagPresentVSync] >>= catchRisky

    walkingAsset <- loadTexture renderer "./assets/walk.png" >>= catchRisky

    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [walkingAsset]
    runStateT (repeatUntilComplete pollDraw) initialState

    freeAssets [walkingAsset]
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    Image.quit


data Key = W | S | N
data ColourProperty = Alpha
data World = World { gameover :: Bool, frame :: Int }
type Input = Maybe SDL.Event
type Asset = (SDL.Texture, CInt, CInt)


fullWindow :: SDL.Rect
fullWindow = toRect 0 0 screenWidth screenHeight


drawState :: SDL.Renderer -> [Asset] -> World -> IO ()
drawState renderer assets (World False frameValue) = withBlankScreen renderer $ do
    let currentFrame = (frameValue `div` 8) `mod` 8
    let (texture, _, _) = head assets
    let spriteRect = toRect 0 0 192 192 

    with2 (getMask currentFrame) (spriteRect `centredOn` fullWindow ) (SDL.renderCopy renderer texture)

    where getMask :: Int -> SDL.Rect
          getMask x = toRect (x * 48) 0 48 48

drawState _ _ _ = return ()


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation
    SDL.renderPresent renderer


updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state { frame = frame state + 1 }


repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) $ repeatUntilComplete game


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

freeAssets :: [Asset] -> IO ()
freeAssets = mapM_ (SDL.destroyTexture . first)
    where first (a, _, _) = a


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

instance Num (GeomPoint) where
   (ax, ay) + (bx, by) = (ax + bx, ay + by)
   (ax, ay) - (bx, by) = (ax - bx, ay - by)
   (ax, ay) * (bx, by) = (ax * bx, ay * by)
   abs (x, y) = (abs x, abs y)
   signum (x, y) = (signum x, signum y)
   fromInteger a = (fromInteger a, 0)


type GeomPoint = (CInt, CInt)


toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect { rectX = fromIntegral x, rectY = fromIntegral y, rectW = fromIntegral w, rectH = fromIntegral h }


moveTo :: (Integral a1, Integral a2) => SDL.Rect -> (a1, a2) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = fromIntegral x, rectY = fromIntegral y }


moveBy :: (Integral a1, Integral a2) => SDL.Rect -> (a1, a2) -> SDL.Rect
moveBy shape (x, y) = shape { rectX = rectX shape + fromIntegral x, rectY = rectY shape + fromIntegral y }


centredOn :: SDL.Rect -> SDL.Rect -> SDL.Rect
centredOn inner outer = inner `moveBy` (centreOf outer - centreOf inner)


centreOf :: SDL.Rect -> GeomPoint
centreOf shape = (x, y)
    where x = rectX shape + rectW shape `div` 2
          y = rectY shape + rectH shape `div` 2


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


infixl 4 ~>>
(~>>) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m b
(~>>) m f = m >>= lift . f


infixl 4 ~>~
(~>~) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m a
(~>~) m f = m >>= \x -> (lift . f) x >> return x


into :: (Monad m, MonadTrans t, MonadState b (t m)) => m a -> (a -> b -> b) -> t m b
into source f = lift source >>= modify . f >> get
