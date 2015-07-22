{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import Control.Monad.State hiding (state)
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Word
import Graphics.UI.SDL.Types
import Shared.Drawing
import Shared.Image
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Textures
import Shared.Utils
import Shared.UtilsState
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image


title :: String
title = "lesson19"

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

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    texture <- loadTexture renderer "./assets/arrow.png"
    gameController <- SDL.gameControllerOpen 0
    if gameController == nullPtr then fail "no controller found" else print "yay!"
    disableEventPolling [SDL.SDL_CONTROLLERAXISMOTION, SDL.SDL_JOYAXISMOTION]
    let initialState = World { gameover = False, getController = gameController, target = (320, 240) }
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [texture]
    _ <- runStateT (repeatUntilComplete pollDraw) initialState
    SDL.gameControllerClose gameController
    SDL.destroyTexture texture
    SDL.destroyRenderer renderer


data World = World { gameover :: Bool, getController :: SDL.GameController, target :: (CInt, CInt) }


disableEventPolling :: [Word32] -> IO ()
disableEventPolling = mapM_ (`SDL.eventState` 0)


drawState :: SDL.Renderer -> [SDL.Texture] -> World -> IO ()
drawState renderer assets state = withBlankScreen renderer $ do
    inputState <- getControllerState (getController state)
    with2 mask (position inputState) $ \mask' position' ->
        SDL.renderCopyEx renderer texture mask' position' degrees' nullPtr SDL.SDL_FLIP_NONE
    where texture = head assets
          w = 89
          h = 50
          sprite = toRect 0 0 w h
          mask = sprite
          position grrr = sprite `centredOn` fullWindow `moveBy` superScale grrr
          degrees' = 0

superScale :: (Double, Double) -> (Int, Int)
superScale = pairMap $ floor . (*) 200

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

getControllerState :: SDL.GameController -> IO (Double, Double)
getControllerState controller = do
    xValue <- getAxisState controller 0
    yValue <- getAxisState controller 1
    let range = hypotenuse xValue yValue
    let deadZone = 8000 ^ (2 :: Integer)
    let carpetValue = if range < deadZone
        then (0, 0)
        else pairMap ssscale (xValue, yValue)
    return carpetValue
      where ssscale x = fromIntegral x / 32768

hypotenuse :: (Num a) => a -> a -> a
hypotenuse a b = a ^ two + b ^ two
  where two = 2 :: Integer

getAxisState :: SDL.GameController -> SDL.GameControllerAxis -> IO Int
getAxisState controller index = do
    axis <- SDL.gameControllerGetAxis controller index
    return $ fromIntegral axis

updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) (repeatUntilComplete game)

instance Num (CInt, CInt) where
   (ax, ay) + (bx, by) = (ax + bx, ay + by)
   (ax, ay) - (bx, by) = (ax - bx, ay - by)
   (ax, ay) * (bx, by) = (ax * bx, ay * by)
   abs (x, y) = (abs x, abs y)
   signum (x, y) = (signum x, signum y)
   fromInteger a = (fromInteger a, 0)

toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect { rectX = fromIntegral x, rectY = fromIntegral y, rectW = fromIntegral w, rectH = fromIntegral h }

moveBy :: (Integral a) => SDL.Rect -> (a, a) -> SDL.Rect
moveBy shape (dx, dy) = shape { rectX = rectX shape + fromIntegral dx, rectY = rectY shape + fromIntegral dy }

centredOn :: SDL.Rect -> SDL.Rect -> SDL.Rect
centredOn inner outer = inner `moveBy` (centreOf outer - centreOf inner)

centreOf :: SDL.Rect -> (CInt, CInt)
centreOf shape = (x, y)
    where x = rectX shape + rectW shape `div` 2
          y = rectY shape + rectH shape `div` 2

