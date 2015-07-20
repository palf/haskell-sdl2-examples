module Main where

import Foreign.C.Types
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
    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    fadeInAsset@(fadeInTexture, _, _) <- loadTexture renderer "./assets/fadein.png" >>= either throwSDLError return
    fadeOutAsset@(fadeOutTexture, _, _) <- loadTexture renderer "./assets/fadeout.png" >>= either throwSDLError return
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [fadeInAsset, fadeOutAsset]
    runStateT (repeatUntilComplete pollDraw) initialState
    destroyTextures [fadeInTexture, fadeOutTexture]
    SDL.destroyRenderer renderer

data Key = W | S | N
data ColourProperty = Alpha
data World = World { gameover :: Bool, alpha :: Word8 }
type Input = Maybe SDL.Event
type Asset = (SDL.Texture, CInt, CInt)

drawState :: SDL.Renderer -> [Asset] -> World -> IO ()
drawState renderer assets (World False alphaValue) = withBlankScreen renderer $ do
    let (background, _, _) = head assets
    let (foreground, _, _) = assets !! 1
    with fullWindow $ SDL.renderCopy renderer background nullPtr
    SDL.setTextureAlphaMod foreground alphaValue
    with fullWindow $ SDL.renderCopy renderer foreground nullPtr
drawState _ _ _ = return ()

withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation
    SDL.renderPresent renderer

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

getKey :: SDL.Keysym -> Key
getKey keysym = case keysymScancode keysym of
    26 -> W
    22 -> S
    _  -> N

destroyTextures :: [SDL.Texture] -> IO ()
destroyTextures = mapM_ SDL.destroyTexture

loadTexture :: SDL.Renderer -> String -> IO (Either String (SDL.Texture, CInt, CInt))
loadTexture renderer path = Image.imgLoadTexture renderer path >>= return . fmap getSize

getSize :: SDL.Texture -> (SDL.Texture, CInt, CInt)
getSize tex = (tex, 640, 480)

renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> SDL.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.renderCopy renderer texture

moveTo :: SDL.Rect -> (CInt, CInt) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = x, rectY = y }

