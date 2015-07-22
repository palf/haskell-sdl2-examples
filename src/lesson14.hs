{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
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
    setHint "SDL_RENDER_SCALE_QUALITY" "0" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    walkingAsset <- loadTexture renderer "./assets/walk.png"
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [walkingAsset]
    runStateT (repeatUntilComplete pollDraw) initialState
    freeAssets [walkingAsset]
    SDL.destroyRenderer renderer

data ColourProperty = Alpha
data World = World { gameover :: Bool, frame :: Int }
type Asset = (SDL.Texture, CInt, CInt)

drawState :: SDL.Renderer -> [Asset] -> World -> IO ()
drawState renderer assets (World False frameValue) = withBlankScreen renderer $ do
    let currentFrame = (frameValue `div` 8) `mod` 8
    let (texture, _, _) = head assets
    let spriteRect = toRect 0 0 192 192 
    with2 (getMask currentFrame) (spriteRect `centredOn` fullWindow ) (SDL.renderCopy renderer texture)
    where getMask :: Int -> SDL.Rect
          getMask x = toRect (x * 48) 0 48 48
drawState _ _ _ = return ()

withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation
    SDL.renderPresent renderer

updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state { frame = frame state + 1 }

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) $ repeatUntilComplete game

freeAssets :: [Asset] -> IO ()
freeAssets = mapM_ (SDL.destroyTexture . first)
    where first (a, _, _) = a

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

