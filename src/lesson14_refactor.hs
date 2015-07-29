{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ConstraintKinds      #-}

module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import Shared.DrawingSimple
import Shared.Geometry
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities
import Shared.State


---- Config ----

title :: String
title = "lesson14"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

initialState :: World
initialState = World { appState = Running, frame = 0 }


data AppState = Running | Exiting deriving Eq
data World = World { appState :: AppState, frame :: Int }


---- Application ----

main :: IO ()
main = withSDLContext $ \renderer ->
    withAssets renderer ["./assets/walk.png"] $ \assets -> do
        let inputSource = pollEvent `into` updateState
        let pollDraw = inputSource ~>~ drawWorld renderer assets
        runStateT (repeatUntilComplete pollDraw) initialState


fullWindow :: SDL.Rect
fullWindow = toRect 0 0 screenWidth screenHeight


drawWorld :: SDL.Renderer -> [Asset] -> World -> IO ()
drawWorld renderer assets (World Running frameValue) = withBlankScreen renderer $ do
    let currentFrame = getFrameAtTime frameValue
    let ImageAsset texture' = head assets
    let spriteRect = toRect 0 0 192 (192 :: Int)

    with2 (getMask currentFrame) (spriteRect `centredOn` fullWindow) (SDL.renderCopy renderer texture')

    where getMask :: Int -> SDL.Rect
          getMask x = toRect (x * 48) 0 48 48
          frameCount = 8
          getFrameAtTime t = 8 `stepsPerSecond` t `mod` frameCount

drawWorld _ _ _ = return ()


stepsPerSecond :: Int -> Int -> Int
stepsPerSecond ms s = (s * ms) `div` 60


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- operation
    SDL.renderPresent renderer


updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState es world = foldl applyEvent world es

applyEvent :: World -> SDL.Event -> World
applyEvent state (SDL.QuitEvent _ _) = state { appState = Exiting }
applyEvent state _ = state { frame = frame state + 1 }


repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \world -> unless (appState world == Exiting) $ repeatUntilComplete game


---- Initialization ----

type SDLInitFlags = Word32
type SDLImageFlags = Image.InitFlag

data SDLHintKey = RenderScaleQuality

instance Show SDLHintKey where
    show RenderScaleQuality = "SDL_RENDER_SCALE_QUALITY"


class SDLHintValue a where
    toString :: a -> String

data RenderScaleQualityValue = Nearest | Linear | Best

instance SDLHintValue RenderScaleQualityValue where
    toString Nearest = "0"
    toString Linear = "1"
    toString Best = "2"

data Asset = ImageAsset SDL.Texture


withSDLContext :: (SDL.Renderer -> IO a) -> IO ()
withSDLContext renderOperation = do
    setupResult <- runExceptT $ do
        initializeSDL [SDL.SDL_INIT_VIDEO]
        Image.imgInit [Image.InitPNG]
        setHint RenderScaleQuality Nearest

        window <- createWindow title
        renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC]
        return (window, renderer)

    sdlCleanup setupResult renderOperation


sdlCleanup :: Either SDLError (SDL.Window, SDL.Renderer) -> (SDL.Renderer -> IO a) -> IO ()
sdlCleanup (Left someError) _ = pollForQuitSDLError error someError
sdlCleanup (Right (window, renderer)) renderOperation = do
    _ <- renderOperation renderer
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    Image.imgQuit


initializeSDL' :: [Word32] -> SDLRisky ()
initializeSDL' flags = do
    result <- liftIO (SDL.init $ foldl (.|.) 0 flags)
    when (result < 0) $ throwSDLError (SDLInitError "SDL")


initializeSDL :: [Word32] -> SDLRisky ()
initializeSDL flags =
    liftIO (SDL.init $ foldl (.|.) 0 flags) >>=
    void . checkForSDLError (SDLInitError "SDL")


class    SDLResult a       where checkForSDLError :: SDLError -> a -> SDLRisky a
instance SDLResult CInt    where checkForSDLError = throwSDLErrorIf (< 0)
instance SDLResult (Ptr a) where checkForSDLError = throwSDLErrorIf (== nullPtr)
instance SDLResult Bool    where checkForSDLError = throwSDLErrorIf not


throwSDLErrorIf :: (a -> Bool) -> SDLError -> a -> SDLRisky a
throwSDLErrorIf isError e result = if isError result
    then throwSDLError e
    else return result

createWindow :: String -> SDLRisky SDL.Window
createWindow windowTitle = do
    window <- liftIO $ withCAString windowTitle $ \title ->
        SDL.createWindow
            title
            SDL.SDL_WINDOWPOS_UNDEFINED
            SDL.SDL_WINDOWPOS_UNDEFINED
            screenWidth
            screenHeight
            SDL.SDL_WINDOW_SHOWN

    if window == nullPtr
        then throwSDLError WindowError
        else return window


createRenderer :: SDL.Window -> CInt -> [Word32] -> SDLRisky SDL.Renderer
createRenderer window index flags = do
    renderer <- liftIO $ SDL.createRenderer window index $ foldl (.|.) 0 flags
    if renderer == nullPtr
        then throwSDLError RendererError
        else return renderer


---- Error Handling ----

type SDLRisky = ExceptT SDLError IO
data SDLError = SDLInitError String
              | SetHintError String
              | WindowError
              | RendererError
              | SurfaceError String
              | TextureError


pollForQuitSDLError :: (MonadIO m) => (String -> IO a) -> SDLError -> m a
pollForQuitSDLError pollForQuit errorType = do
    let message = getSDLErrorMessage errorType
    errorString <- liftIO (SDL.getError >>= peekCString)
    liftIO $ pollForQuit (message ++ " SDL_Error: " ++ errorString)


logSDLError :: (MonadIO m) => SDLError -> m ()
logSDLError = pollForQuitSDLError print


throwSDLError :: SDLError -> SDLRisky a
throwSDLError = ExceptT . return . Left


getSDLErrorMessage :: SDLError -> String
getSDLErrorMessage (SDLInitError name)  = name ++ " could not initialize!"
getSDLErrorMessage (SetHintError hint)  = "Hint '" ++ hint ++ "' could not be applied!"
getSDLErrorMessage WindowError          = "Window could not be created!"
getSDLErrorMessage RendererError        = "Renderer could not be created!"
getSDLErrorMessage (SurfaceError path)  = "Unable to load image '" ++ path ++ "'!"
getSDLErrorMessage TextureError         = "Unable to create texture!"


