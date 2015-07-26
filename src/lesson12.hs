module Main (main) where

import Foreign.Ptr
import Control.Monad.State hiding (state)
import GHC.Word
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities
import Shared.State

title :: String
title = "lesson12"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

initialState :: World
initialState = World { gameover = False, red = 128, green = 128, blue = 128 }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    withAssets renderer ["./assets/colors.png"] $ \assets -> do
        let inputSource = pollEvent `into` updateState
        let pollDraw = inputSource ~>> drawWorld renderer assets
        runStateT (repeatUntilComplete pollDraw) initialState
    SDL.destroyRenderer renderer

data Colour = Red | Green | Blue
data World = World { gameover :: Bool, red :: Word8, green :: Word8, blue :: Word8 }

--Input -> Intent -> World ->? Render
--keypress -> increaseRed -> updateWorld -> draw

drawWorld :: SDL.Renderer -> [Asset] -> World -> IO World
drawWorld renderer assets state@(World False r g b) = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- SDL.setTextureColorMod texture r g b
    _ <- renderTexture' position
    _ <- SDL.renderPresent renderer
    return state
    where (texture, width, height) = head assets
          renderTexture' renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr
          position = SDL.Rect { rectX = 0, rectY = 0, rectW = width, rectH = height }
drawWorld _ _ state = return state

updateState :: Input -> World -> World
updateState (Just (SDL.KeyboardEvent evtType _ _ _ _ keysym)) state = if evtType == SDL.SDL_KEYDOWN then modifyState state keysym else state
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

