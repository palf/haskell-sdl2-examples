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

initialState :: World
initialState = World { gameover = False, degrees = 0, flipType = SDL.SDL_FLIP_NONE }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    withAssets renderer ["./assets/arrow.png"] $ \assets -> do
        let inputSource = pollEvent `into` updateState
        let pollDraw = inputSource ~>~ drawWorld renderer assets
        runStateT (repeatUntilComplete pollDraw) initialState
    SDL.destroyRenderer renderer

data World = World { gameover :: Bool, degrees :: Int, flipType :: SDL.RendererFlip }

drawWorld :: SDL.Renderer -> [Asset] -> World -> IO ()
drawWorld renderer assets state = withBlankScreen renderer $
    with2 mask position $ \mask' position' ->
        SDL.renderCopyEx renderer texture mask' position' degrees' nullPtr (flipType state)

    where (texture, w, h) = head assets
          sprite = toRect 0 0 w h
          mask = sprite
          position = sprite `centredOn` fullWindow
          degrees' = fromIntegral (degrees state)

updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState es world = foldl applyEvent world es

applyEvent :: World -> SDL.Event -> World
applyEvent state (SDL.QuitEvent _ _) = state { gameover = True }
applyEvent state (SDL.KeyboardEvent evtType _ _ _ _ keysym) = if evtType == SDL.SDL_KEYDOWN
    then modifyState state keysym
    else state
applyEvent state _ = state

modifyState :: World -> SDL.Keysym -> World
modifyState state keysym = case getKey keysym of
    Q -> state { flipType = SDL.SDL_FLIP_HORIZONTAL }
    W -> state { flipType = SDL.SDL_FLIP_NONE }
    E -> state { flipType = SDL.SDL_FLIP_VERTICAL }
    A -> state { degrees = degrees state - 15 }
    D -> state { degrees = degrees state + 15 }
    S -> state { degrees = 0, flipType = SDL.SDL_FLIP_NONE }
    _ -> state

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) $ repeatUntilComplete game

