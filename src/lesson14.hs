{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Shared.Input
import Shared.Drawing
import Shared.Lifecycle
import Shared.Geometry
import Shared.Polling
import Shared.Utils
import Shared.Textures
import Shared.UtilsState


title :: String
title = "lesson14"

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

--fullWindow :: SDL.Rect
--fullWindow = toRect 0 0 screenWidth screenHeight

initialState :: World
initialState = World { gameover = False, frame = 0 }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "0" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    walkingTexture <- loadTexture renderer "./assets/walk.png"
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [walkingTexture]
    _ <- runStateT (repeatUntilComplete pollDraw) initialState
    destroyTextures [walkingTexture]
    SDL.destroyRenderer renderer

data World = World { gameover :: Bool, frame :: Int }

drawState :: SDL.Renderer -> [SDL.Texture] -> World -> IO ()
drawState renderer assets (World False frameValue) = withBlankScreen renderer $ do
    let currentFrame = (frameValue `div` 8) `mod` 8
    let texture = head assets
    let spriteRect = toRect 0 0 (192::Integer) (192::Integer)
    with2 (getMask currentFrame) (spriteRect `centredOn` fullWindow ) (SDL.renderCopy renderer texture)
    where getMask :: Int -> SDL.Rect
          getMask x = toRect (x * 48) 0 48 48
drawState _ _ _ = return ()

updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state { frame = frame state + 1 }

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) $ repeatUntilComplete game

