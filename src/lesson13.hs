module Main (main) where

import Control.Monad.State hiding (state)
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Word
import Graphics.UI.SDL.Types
import Shared.Drawing
import Shared.Image
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Textures
import Shared.Utilities
import Shared.State
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image


title :: String
title = "lesson13"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

fullWindow :: SDL.Rect
fullWindow = SDL.Rect {
    rectX = 0,
    rectY = 0,
    rectW = fst size,
    rectH = snd size }

initialState :: World
initialState = World { gameover = False, alpha = 128 }


main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    textures <- mapM (loadTextureAsSurface renderer) ["./assets/fadein.png", "./assets/fadeout.png"]
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawWorld renderer textures
    _ <- runStateT (repeatUntilComplete pollDraw) initialState
    destroyTextures textures
    SDL.destroyRenderer renderer


data ColourProperty = Alpha
data World = World { gameover :: Bool, alpha :: Word8 }


drawWorld :: SDL.Renderer -> [SDL.Texture] -> World -> IO ()
drawWorld renderer assets (World False alphaValue) = withBlankScreen renderer $ do
    let background = head assets
    let foreground = assets !! 1
    _ <- with fullWindow $ SDL.renderCopy renderer background nullPtr
    _ <- SDL.setTextureAlphaMod foreground alphaValue
    with fullWindow $ SDL.renderCopy renderer foreground nullPtr
drawWorld _ _ _ = return ()

updateState :: Input -> World -> World
updateState (Just (SDL.KeyboardEvent evtType _ _ _ _ keysym)) state = if evtType == SDL.SDL_KEYDOWN then modifyState state keysym else state
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state

modifyState :: World -> SDL.Keysym -> World
modifyState state keysym = case getKey keysym of
    W -> state `increase` Alpha
    S -> state `decrease` Alpha
    _ -> state

increase :: World -> ColourProperty -> World
increase state Alpha = state { alpha = alpha state + 16 }

decrease :: World -> ColourProperty -> World
decrease state Alpha = state { alpha = alpha state - 16 }

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = do
    state <- game
    unless (gameover state) $ repeatUntilComplete game

loadTextureAsSurface :: SDL.Renderer -> String -> IO SDL.Texture
loadTextureAsSurface renderer path = do
    loadedSurface <- imgLoadSurface path >>= either throwSDLError return
    let applyToSurface = flip applyToPointer loadedSurface
    pixelFormat <- applyToSurface SDL.surfaceFormat
    key <- SDL.mapRGB pixelFormat 0 0xFF 0xFF
    _ <- SDL.setColorKey loadedSurface 1 key
    newTexture <- createTextureFromSurface renderer loadedSurface >>= either throwSDLError return
    SDL.freeSurface loadedSurface
    return newTexture

