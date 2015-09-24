module Main (main) where

import Control.Monad.State hiding (state)
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Word
import Graphics.UI.SDL.Types
import Shared.Drawing
import Shared.Input
import Shared.Lifecycle
import Shared.Assets
import Shared.Polling
import Shared.Utilities
import Shared.State
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image


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

initialWorld :: World
initialWorld = World { gameover = False, alpha = 0 }


main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    withAssets renderer ["./assets/fadein.png", "./assets/fadeout.png"] $ \assets -> do
        let inputSource = pollEvent `into` updateState
        let pollDraw = inputSource ~>~ drawWorld renderer assets
        runStateT (repeatUntilComplete pollDraw) initialWorld
    SDL.destroyRenderer renderer


data ColourProperty = Alpha
data World = World { gameover :: Bool, alpha :: Word8 }

drawWorld :: SDL.Renderer -> [Asset] -> World -> IO ()
drawWorld renderer assets (World False alphaValue) = withBlankScreen renderer $ do
    let (background, _, _) = head assets
    let (foreground, _, _) = assets !! 1
    _ <- SDL.setTextureAlphaMod foreground alphaValue
    _ <- with fullWindow $ SDL.renderCopy renderer background nullPtr
    _ <- with fullWindow $ SDL.renderCopy renderer foreground nullPtr
    return ()
drawWorld _ _ _ = return ()

updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState events world = foldl applyEvent world events

applyEvent :: World -> SDL.Event -> World
applyEvent world (SDL.KeyboardEvent evtType _ _ _ _ keysym)
  | evtType == SDL.SDL_KEYDOWN = modifyState world keysym
  | otherwise = world
applyEvent world (SDL.QuitEvent _ _) = world { gameover = True }
applyEvent world _ = world

modifyState :: World -> SDL.Keysym -> World
modifyState world keysym = case getKey keysym of
    W -> world `increase` Alpha
    S -> world `decrease` Alpha
    _ -> world

increase :: World -> ColourProperty -> World
increase world Alpha = world { alpha = alpha world + 16 }

decrease :: World -> ColourProperty -> World
decrease world Alpha = world { alpha = alpha world - 16 }

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = do
    world <- game
    unless (gameover world) $ repeatUntilComplete game

