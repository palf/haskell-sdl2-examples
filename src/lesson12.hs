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
drawWorld renderer assets world@(World False r g b) = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- SDL.setTextureColorMod texture r g b
    _ <- renderTexture' position
    _ <- SDL.renderPresent renderer
    return world
    where (texture, width, height) = head assets
          renderTexture' renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr
          position = SDL.Rect { rectX = 0, rectY = 0, rectW = width, rectH = height }
drawWorld _ _ world = return world

updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState events world = foldl applyEvent world events

applyEvent :: World -> SDL.Event -> World
applyEvent world (SDL.QuitEvent _ _) = world { gameover = True }
applyEvent world (SDL.KeyboardEvent evtType _ _ _ _ keysym)
 | evtType == SDL.SDL_KEYDOWN = modifyState world keysym 
 | otherwise                  = world
applyEvent world _ = world

modifyState :: World -> SDL.Keysym -> World
modifyState world keysym = case getKey keysym of
    Q -> world `increase` Red
    W -> world `increase` Green
    E -> world `increase` Blue
    A -> world `decrease` Red
    S -> world `decrease` Green
    D -> world `decrease` Blue
    _ -> world

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = do
    world <- game
    unless (gameover world) $ repeatUntilComplete game

increase :: World -> Colour -> World
increase world Red = world { red = red world + 16 }
increase world Green = world { green = green world + 16 }
increase world Blue = world { blue = blue world + 16 }

decrease :: World -> Colour -> World
decrease world Red = world { red = red world - 16 }
decrease world Green = world { green = green world - 16 }
decrease world Blue = world { blue = blue world - 16 }

