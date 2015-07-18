{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Foreign.C.Types
import Foreign.Ptr
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils
import Shared.UtilsState


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
    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    asset <- loadTexture renderer "./assets/arrow.png" >>= either throwSDLError return
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [asset]
    runStateT (repeatUntilComplete pollDraw) initialState
    freeAssets [asset]
    SDL.destroyRenderer renderer

data Key = Q | W | E | A | S | D | N
data ColourProperty = Red | Green | Blue | Alpha
data World = World { gameover :: Bool, degrees :: Int, flipType :: SDL.RendererFlip }
type Input = Maybe SDL.Event
type Asset = (SDL.Texture, CInt, CInt)

drawState :: SDL.Renderer -> [Asset] -> World -> IO ()
drawState renderer assets state = withBlankScreen renderer $
    with2 mask position $ \mask' position' ->
        SDL.renderCopyEx renderer texture mask' position' degrees' nullPtr (flipType state)

    where (texture, w, h) = head assets
          sprite = toRect 0 0 w h
          mask = sprite
          position = sprite `centredOn` fullWindow
          degrees' = fromIntegral (degrees state)

withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation
    SDL.renderPresent renderer

updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState (Just (SDL.KeyboardEvent evtType _ _ _ _ keysym)) state = if evtType == SDL.SDL_KEYDOWN
    then modifyState state keysym
    else state
updateState _ state = state

modifyState :: World -> SDL.Keysym -> World
modifyState state keysym = case getKey keysym of
    Q -> state { flipType = SDL.SDL_FLIP_HORIZONTAL }
    W -> state { flipType = SDL.SDL_FLIP_NONE }
    E -> state { flipType = SDL.SDL_FLIP_VERTICAL }
    A -> state { degrees = degrees state - 15 }
    D -> state { degrees = degrees state + 15 }
    S -> state { degrees = 0, flipType = SDL.SDL_FLIP_NONE }
    _ -> state

getKey :: SDL.Keysym -> Key
getKey keysym = case keysymScancode keysym of
    20 -> Q
    26 -> W
    8  -> E
    4  -> A
    22 -> S
    7  -> D
    _  -> N

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) $ repeatUntilComplete game

freeAssets :: [Asset] -> IO ()
freeAssets = mapM_ (SDL.destroyTexture . first)
    where first (a, _, _) = a

loadTexture :: SDL.Renderer -> String -> IO (Either String (SDL.Texture, CInt, CInt))
loadTexture renderer path = Image.imgLoadTexture renderer path >>= return . fmap fakeSize

fakeSize :: SDL.Texture -> (SDL.Texture, CInt, CInt)
fakeSize tex = (tex, 178, 100)

renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> SDL.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.renderCopy renderer texture

instance Num (GeomPoint) where
   (ax, ay) + (bx, by) = (ax + bx, ay + by)
   (ax, ay) - (bx, by) = (ax - bx, ay - by)
   (ax, ay) * (bx, by) = (ax * bx, ay * by)
   abs (x, y) = (abs x, abs y)
   signum (x, y) = (signum x, signum y)
   fromInteger a = (fromInteger a, 0)

type GeomPoint = (CInt, CInt)

toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect { rectX = fromIntegral x, rectY = fromIntegral y, rectW = fromIntegral w, rectH = fromIntegral h }

moveTo :: (Integral a1, Integral a2) => SDL.Rect -> (a1, a2) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = fromIntegral x, rectY = fromIntegral y }

moveBy :: (Integral a1, Integral a2) => SDL.Rect -> (a1, a2) -> SDL.Rect
moveBy shape (x, y) = shape { rectX = rectX shape + fromIntegral x, rectY = rectY shape + fromIntegral y }

centredOn :: SDL.Rect -> SDL.Rect -> SDL.Rect
centredOn inner outer = inner `moveBy` (centreOf outer - centreOf inner)

centreOf :: SDL.Rect -> GeomPoint
centreOf shape = (x, y)
    where x = rectX shape + rectW shape `div` 2
          y = rectY shape + rectH shape `div` 2

toSDLPoint :: GeomPoint -> SDL.Point
toSDLPoint (x, y) = SDL.Point { pointX = x, pointY = y }

