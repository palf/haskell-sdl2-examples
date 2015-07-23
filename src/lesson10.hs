module Main (main) where

import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Textures
import Shared.Utils


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
    fooTexture <- loadTexture renderer "./assets/foo.png"
    (fw, fh) <- getTextureSize fooTexture
    let foo = (fooTexture, fw, fh)
    backgroundTexture <- loadTexture renderer "./assets/background.png"
    (bw, bh) <- getTextureSize backgroundTexture
    let background = (backgroundTexture, bw, bh)
    let instructions = createRenderInstructions background foo
    repeatUntilTrue $ draw renderer instructions >> handleNoInput pollEvent
    destroyTextures [fooTexture, backgroundTexture]
    SDL.destroyRenderer renderer

type RenderInstructions = SDL.Renderer -> IO ()

createRenderInstructions :: (SDL.Texture, CInt, CInt) -> (SDL.Texture, CInt, CInt) -> RenderInstructions
createRenderInstructions (background, bw, bh) (foo, fw, fh) renderer = do
    _ <- renderTextureIn' background backgroundQuad
    _ <- renderTextureIn' foo fooQuad
    return ()
    where renderTextureIn' = renderTextureIn renderer
          backgroundQuad = SDL.Rect { rectX = 0, rectY = 0, rectW = bw, rectH = bh }
          fooQuad = SDL.Rect { rectX = 240, rectY = 190, rectW = fw, rectH = fh }

renderTextureIn :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> IO CInt
renderTextureIn renderer texture renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr

draw :: SDL.Renderer -> RenderInstructions -> IO ()
draw renderer instructions = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- instructions renderer
    SDL.renderPresent renderer

