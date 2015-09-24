{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Foreign.Ptr
import Shared.Assets
import Shared.Geometry
import Shared.Drawing
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities
import Shared.State


title :: String
title = "lesson15"

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

initialWorld :: World
initialWorld = World { gameover = False, degrees = 0, flipType = SDL.SDL_FLIP_NONE }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    withAssets renderer ["./assets/arrow.png"] $ \assets -> do
        let inputSource = pollEvent `into` updateState
        let pollDraw = inputSource ~>~ drawWorld renderer assets
        runStateT (repeatUntilComplete pollDraw) initialWorld
    SDL.destroyRenderer renderer

data World = World { gameover :: Bool, degrees :: Int, flipType :: SDL.RendererFlip }

drawWorld :: SDL.Renderer -> [Asset] -> World -> IO ()
drawWorld renderer assets world = withBlankScreen renderer $
    with2 mask position $ \mask' position' ->
        SDL.renderCopyEx renderer texture mask' position' degrees' nullPtr (flipType world)

    where (texture, w, h) = head assets
          sprite = toRect 0 0 w h
          mask = sprite
          position = sprite `centredOn` fullWindow
          degrees' = fromIntegral (degrees world)

updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState events world = foldl applyEvent world events

applyEvent :: World -> SDL.Event -> World
applyEvent world (SDL.QuitEvent _ _) = world { gameover = True }
applyEvent world (SDL.KeyboardEvent evtType _ _ _ _ keysym) = if evtType == SDL.SDL_KEYDOWN
    then modifyState world keysym
    else world
applyEvent world _ = world

modifyState :: World -> SDL.Keysym -> World
modifyState world keysym = case getKey keysym of
    Q -> world { flipType = SDL.SDL_FLIP_HORIZONTAL }
    W -> world { flipType = SDL.SDL_FLIP_NONE }
    E -> world { flipType = SDL.SDL_FLIP_VERTICAL }
    A -> world { degrees = degrees world - 15 }
    D -> world { degrees = degrees world + 15 }
    S -> world { degrees = 0, flipType = SDL.SDL_FLIP_NONE }
    _ -> world

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \world -> unless (gameover world) $ repeatUntilComplete game

