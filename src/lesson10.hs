module Main where

import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Assets
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson10"

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
    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    foo@(fooTexture, _, _) <- loadTexture renderer "./assets/foo.png" >>= either throwSDLError return
    background@(backgroundTexture, _, _) <- loadTexture renderer "./assets/background.png" >>= either throwSDLError return
    let instructions = createRenderInstructions background foo
    repeatUntilTrue $ draw renderer instructions >> handleNoInput pollEvent
    destroyTextures [fooTexture, backgroundTexture]
    SDL.destroyRenderer renderer

type RenderInstructions = SDL.Renderer -> IO ()

createRenderInstructions :: (SDL.Texture, CInt, CInt) -> (SDL.Texture, CInt, CInt) -> RenderInstructions
createRenderInstructions (background, bw, bh) (foo, fw, fh) renderer = do
    renderTextureIn' background backgroundQuad
    renderTextureIn' foo fooQuad
    return ()
    where renderTextureIn' = renderTextureIn renderer
          backgroundQuad = SDL.Rect { rectX = 0, rectY = 0, rectW = bw, rectH = bh }
          fooQuad = SDL.Rect { rectX = 240, rectY = 190, rectW = fw, rectH = fh }

destroyTextures :: [SDL.Texture] -> IO ()
destroyTextures = mapM_ SDL.destroyTexture

loadTexture :: SDL.Renderer -> String -> IO (Either String (SDL.Texture, CInt, CInt))
loadTexture renderer path = Image.imgLoadTexture renderer path >>= return . fmap getSize

getSize :: SDL.Texture -> (SDL.Texture, CInt, CInt)
getSize tex = (tex, 640, 480)

getTextureSize :: SDL.Texture -> IO (CInt, CInt)
getTextureSize tex = do
    query <- SDL.queryTexture tex format access width height
    return (width, height)

renderTextureIn :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> IO CInt
renderTextureIn renderer texture renderQuad = with renderQuad $ SDL.renderCopy renderer texture nullPtr

draw :: SDL.Renderer -> RenderInstructions -> IO ()
draw renderer instructions = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    instructions renderer
    SDL.renderPresent renderer

