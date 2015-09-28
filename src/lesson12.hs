module Main (main) where

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
import Shared.Utilities
import Shared.Drawing

title :: String
title = "lesson12"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

initialWorld :: World
initialWorld = World { gameover = False, red = 128, green = 128, blue = 128 }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    withAssets renderer ["./assets/colors.png"] $ runGame renderer
    SDL.destroyRenderer renderer

data World = World { gameover :: Bool, red :: Word8, green :: Word8, blue :: Word8 } deriving (Show)
data Colour = Red | Green | Blue
data Intent = Increase Colour | Decrease Colour | DoNothing | Quit
type UpdateWorld = World -> World

runGame :: SDL.Renderer -> [Asset] -> IO ()
runGame renderer assets = repeatUntilGameover updateSource drawWorld initialWorld
    where drawWorld = draw renderer assets

repeatUntilGameover :: (Monad m) => m UpdateWorld -> (World -> m ()) -> World -> m ()
repeatUntilGameover updateFunc drawFunc = go
  where go world = updateFunc <*> pure world >>= \world' ->
          drawFunc world' >> unless (gameover world') (go world')

updateSource :: IO UpdateWorld
updateSource = createUpdateFunction collectEvents

createUpdateFunction :: (Monad m, Functor f, Foldable f) => m (f SDL.Event) -> m UpdateWorld
createUpdateFunction input = do
    events <- input
    let intents = fmap eventToIntent events
    return $ \world -> foldl (flip applyIntent) world intents

eventToIntent :: SDL.Event -> Intent
eventToIntent (SDL.QuitEvent _ _) = Quit
eventToIntent (SDL.KeyboardEvent evtType _ _ _ _ keysym)
 | evtType == SDL.SDL_KEYDOWN = inputToIntent (getKey keysym)
 | otherwise                  = DoNothing
eventToIntent _ = DoNothing

inputToIntent :: KeyPress -> Intent
inputToIntent key = case key of
    Q -> Increase Red
    W -> Increase Green
    E -> Increase Blue
    A -> Decrease Red
    S -> Decrease Green
    D -> Decrease Blue
    _ -> DoNothing

applyIntent :: Intent -> UpdateWorld
applyIntent (Increase color) = increase color
applyIntent (Decrease color) = decrease color
applyIntent DoNothing = id
applyIntent Quit = quit

increase :: Colour -> UpdateWorld
increase Red world = world { red = red world + 16 }
increase Green world = world { green = green world + 16 }
increase Blue world = world { blue = blue world + 16 }

decrease :: Colour -> UpdateWorld
decrease Red world = world { red = red world - 16 }
decrease Green world = world { green = green world - 16 }
decrease Blue world = world { blue = blue world - 16 }

quit :: UpdateWorld
quit world = world { gameover = True }

draw :: SDL.Renderer -> [Asset] -> World -> IO ()
draw renderer assets world = do
    let instructions = writeRenderInstructions assets world
    withBlankScreen renderer $ executeRender renderer instructions

data RenderInstruction =
    SetTextureColor SDL.Texture Word8 Word8 Word8 |
    RenderCopy SDL.Texture SDL.Rect

writeRenderInstructions :: [Asset] -> World -> [RenderInstruction]
writeRenderInstructions assets (World _ r g b) = [
    SetTextureColor texture r g b ,
    RenderCopy texture position ]
    where (texture, width, height) = head assets
          position = SDL.Rect { rectX = 0, rectY = 0, rectW = width, rectH = height }

executeRender :: (Foldable f) => SDL.Renderer -> f RenderInstruction -> IO ()
executeRender renderer = mapM_ (executeInstruction renderer)

executeInstruction :: SDL.Renderer -> RenderInstruction -> IO ()
executeInstruction _ (SetTextureColor texture r g b) = void $ SDL.setTextureColorMod texture r g b
executeInstruction renderer (RenderCopy texture position) = void $ with position $ SDL.renderCopy renderer texture nullPtr

