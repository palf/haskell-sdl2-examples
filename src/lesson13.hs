module Main where

import Control.Monad.State hiding (state)
import GHC.Word
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Textures
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils
import Shared.UtilsState

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
initialState = World { gameover = False, alpha = 64 }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    assets <- mapM (loadTexture renderer) ["./assets/fadein.png", "./assets/fadeout.png"]
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer assets
    _ <- runStateT (repeatUntilComplete pollDraw) initialState
    destroyTextures assets
    SDL.destroyRenderer renderer

data ColourProperty = Alpha
data World = World { gameover :: Bool, alpha :: Word8 }
type Input = Maybe SDL.Event

drawState :: SDL.Renderer -> [SDL.Texture] -> World -> IO ()
drawState renderer assets (World False alphaValue) = withBlankScreen renderer $ do
    let background = head assets
    let foreground = assets !! 1
    _ <- SDL.setTextureAlphaMod foreground alphaValue
    _ <- with (leftHalf fullWindow) $ SDL.renderCopy renderer background nullPtr
    _ <- with (rightHalf fullWindow) $ SDL.renderCopy renderer foreground nullPtr
    return ()
drawState _ _ _ = return ()

withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- operation
    _ <- SDL.renderPresent renderer
    return ()

leftHalf :: SDL.Rect -> SDL.Rect
leftHalf rect = rect { rectW = rectW rect `div` 2 }

rightHalf :: SDL.Rect -> SDL.Rect
rightHalf rect = rect {
    rectX = rectX rect + rectW rect `div` 2,
    rectW = rectW rect `div` 2 }


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

