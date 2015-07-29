module Main (main) where

import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Types
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image


title :: String
title = "lesson10"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    withAssets renderer ["./assets/background.png", "./assets/foo.png"] $ \assets -> do
        let instructions = createRenderInstructions assets
        repeatUntilTrue $ draw renderer instructions >> pollForQuit pollEvent
    SDL.destroyRenderer renderer

type RenderInstructions = SDL.Renderer -> IO ()

createRenderInstructions :: [Asset] -> RenderInstructions
createRenderInstructions assets renderer = do
    _ <- renderTextureIn' background backgroundQuad
    _ <- renderTextureIn' foreground foregroundQuad
    return ()
    where (background, bw, bh) = head assets
          (foreground, fw, fh) = assets !! 1
          renderTextureIn' = renderTextureIn renderer
          backgroundQuad = SDL.Rect { rectX = 0, rectY = 0, rectW = bw, rectH = bh }
          foregroundQuad = SDL.Rect { rectX = 240, rectY = 190, rectW = fw, rectH = fh }

renderTextureIn :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> IO CInt
renderTextureIn renderer texture renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr

draw :: SDL.Renderer -> RenderInstructions -> IO ()
draw renderer instructions = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- instructions renderer
    SDL.renderPresent renderer

