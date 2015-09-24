{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Shared.Assets
import Shared.Drawing
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities
import Shared.State


title :: String
title = "lesson17"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

initialWorld :: World
initialWorld = World { gameover = False, quadrants = map makeEntity allPositions }

makeEntity :: Position -> Entity
makeEntity pos = Entity { mouseState = MouseOut, position = pos }

main :: IO ()
main = withSDLContext $ \renderer ->
    withAssets renderer ["./assets/mouse_states.png"] $ \assets -> do
        let inputSource = collectEvents `into` updateState
        let pollDraw = inputSource ~>~ drawWorld renderer assets
        runStateT (repeatUntilComplete pollDraw) initialWorld

withSDLContext :: (SDL.Renderer -> IO ()) -> IO ()
withSDLContext f = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "0" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    _ <- f renderer
    SDL.destroyRenderer renderer


data World = World { gameover :: Bool, quadrants :: [Entity] }
data Entity = Entity { mouseState :: EntityState, position :: Position }
data Position = TopLeft | TopRight | BottomLeft | BottomRight deriving (Eq, Enum, Bounded)
data EntityState = MouseOut | MouseOver | MouseDown | MouseUp

drawWorld :: SDL.Renderer -> [Asset] -> World -> IO ()
drawWorld renderer assets world = withBlankScreen renderer $ mapM render' (quadrants world)
    where (texture, w, h) = head assets
          render' entity = with2 (maskFor entity) (positionFor entity) (SDL.renderCopy renderer texture)
          sprite = toRect 0 0 (w `div` 2) (h `div` 2)
          maskFor entity = maskFromState sprite (mouseState entity)
          positionFor entity = sprite `moveTo` positionToPoint (position entity)

within :: (Int, Int) -> (Int, Int) -> Bool
within (mx, my) (px, py) = withinX && withinY
    where withinX = px < mx && mx < px + 320
          withinY = py < my && my < py + 240

maskFromState :: SDL.Rect -> EntityState -> SDL.Rect
maskFromState sprite MouseOut = sprite `moveTo` positionToPoint TopLeft
maskFromState sprite MouseOver = sprite `moveTo` positionToPoint TopRight
maskFromState sprite MouseDown = sprite `moveTo` positionToPoint BottomLeft
maskFromState sprite MouseUp = sprite `moveTo` positionToPoint BottomRight

positionToPoint :: Position -> (Int, Int)
positionToPoint TopLeft = (0, 0)
positionToPoint TopRight = (320, 0)
positionToPoint BottomLeft = (0, 240)
positionToPoint BottomRight = (320, 240)

allPositions :: [Position]
allPositions = [minBound .. ]

updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState events world = foldl applyEvent world events

applyEvent :: World -> SDL.Event -> World
applyEvent world (SDL.QuitEvent _ _) = world { gameover = True }
applyEvent world (SDL.MouseMotionEvent _ _ _ _ _ x y _ _) = world { quadrants = updatedEntities }
    where updatedEntities = map (makeNewEntity (x, y) MouseOver) allPositions
applyEvent world (SDL.MouseButtonEvent evtType _ _ _ _ _ _ x y)
    | evtType == SDL.SDL_MOUSEBUTTONDOWN = world { quadrants = updatedEntities MouseDown }
    | evtType == SDL.SDL_MOUSEBUTTONUP = world { quadrants = updatedEntities MouseUp }
    | otherwise = world
    where updatedEntities ms = map (makeNewEntity (x, y) ms) allPositions
applyEvent world _ = world

makeNewEntity :: (Integral a) => (a, a) -> EntityState -> Position -> Entity
makeNewEntity (x', y') ms pos = Entity { mouseState = newState, position = pos }
    where newState = getMouseState pos (x, y) ms
          (x, y) = (fromIntegral x', fromIntegral y')

getMouseState :: Position -> (Int, Int) -> EntityState -> EntityState
getMouseState pos (x, y) ms
    | (x, y) `within` n = ms
    | otherwise         = MouseOut
    where n = positionToPoint pos

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \world -> unless (gameover world) $ repeatUntilComplete game

toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect { rectX = fromIntegral x, rectY = fromIntegral y, rectW = fromIntegral w, rectH = fromIntegral h }

moveTo :: (Integral a1, Integral a2) => SDL.Rect -> (a1, a2) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = fromIntegral x, rectY = fromIntegral y }

