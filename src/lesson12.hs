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
title = "lesson12"

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
initialState = World { gameover = False, red = 128, green = 128, blue = 128 }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    asset@(texture, _, _) <- loadTexture renderer "./assets/colors.png" >>= either throwSDLError return
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>> drawState renderer asset
    runStateT (repeatUntilComplete pollDraw) initialState
    destroyTextures [texture]
    SDL.destroyRenderer renderer

data Colour = Red | Green | Blue
data Key = Q | W | E | A | S | D | N
data World = World { gameover :: Bool, red :: Word8, green :: Word8, blue :: Word8 }
type Input = Maybe SDL.Event
type Asset = (SDL.Texture, CInt, CInt)

drawState :: SDL.Renderer -> Asset -> World -> IO World
drawState renderer (texture, width, height) state@(World False r g b) = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    SDL.setTextureColorMod texture r g b
    renderTexture' position
    SDL.renderPresent renderer
    return state
    where renderTexture' renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr
          position = SDL.Rect { rectX = 0, rectY = 0, rectW = width, rectH = height }
drawState _ _ state = return state

updateState :: Input -> World -> World
updateState (Just (SDL.KeyboardEvent evtType _ _ _ _ keysym)) state = if evtType == SDL.SDL_KEYDOWN then modifyState state keysym else state
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state

modifyState :: World -> SDL.Keysym -> World
modifyState state keysym = case getKey keysym of
    Q -> state `increase` Red
    W -> state `increase` Green
    E -> state `increase` Blue
    A -> state `decrease` Red
    S -> state `decrease` Green
    D -> state `decrease` Blue
    _ -> state

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = do
    state <- game
    unless (gameover state) $ repeatUntilComplete game

increase :: World -> Colour -> World
increase state Red = state { red = red state + 16 }
increase state Green = state { green = green state + 16 }
increase state Blue = state { blue = blue state + 16 }

decrease :: World -> Colour -> World
decrease state Red = state { red = red state - 16 }
decrease state Green = state { green = green state - 16 }
decrease state Blue = state { blue = blue state - 16 }

getKey :: SDL.Keysym -> Key
getKey keysym = case keysymScancode keysym of
    20 -> Q
    26 -> W
    8  -> E
    4  -> A
    22 -> S
    7  -> D
    _  -> N

destroyTextures :: [SDL.Texture] -> IO ()
destroyTextures = mapM_ SDL.destroyTexture

loadTexture :: SDL.Renderer -> String -> IO (Either String (SDL.Texture, CInt, CInt))
loadTexture renderer path = Image.imgLoadTexture renderer path >>= return . fmap fakeSize

fakeSize :: SDL.Texture -> (SDL.Texture, CInt, CInt)
fakeSize tex = (tex, 640, 480)

renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> SDL.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.renderCopy renderer texture

moveTo :: SDL.Rect -> (CInt, CInt) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = x, rectY = y }

