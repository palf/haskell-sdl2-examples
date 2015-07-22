module Main where

import Foreign.C.Types
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Shared.Textures
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utils


title :: String
title = "lesson11"

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
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    dotsTexture <- loadTexture renderer "./assets/dots.png"
    (dw, dh) <- getTextureSize dotsTexture
    let dots = (dotsTexture, dw, dh)
    let instructions = createRenderInstructions dots
    repeatUntilTrue $ draw renderer instructions >> handleNoInput pollEvent
    destroyTextures [dotsTexture]
    SDL.destroyRenderer renderer

type RenderInstructions = SDL.Renderer -> IO ()

createRenderInstructions :: (SDL.Texture, CInt, CInt) -> RenderInstructions
createRenderInstructions (texture, width, height) renderer = do
    _ <- renderTexture' topLeftMask topLeftPosition
    _ <- renderTexture' topRightMask topRightPosition
    _ <- renderTexture' bottomLeftMask bottomLeftPosition
    _ <- renderTexture' bottomRightMask bottomRightPosition
    return ()

    where renderTexture' = renderTexture renderer texture
          dotMask = SDL.Rect { rectX = 0, rectY = 0, rectW = width `div` 2, rectH = height `div` 2 }
          topLeftMask     = dotMask `moveTo` (  0,   0)
          topRightMask    = dotMask `moveTo` (100,   0)
          bottomLeftMask  = dotMask `moveTo` (  0, 100)
          bottomRightMask = dotMask `moveTo` (100, 100)
          topLeftPosition     = dotMask `moveTo` (0, 0)
          topRightPosition    = dotMask `moveTo` (screenWidth - width `div` 2, 0)
          bottomLeftPosition  = dotMask `moveTo` (0, screenHeight - height `div` 2)
          bottomRightPosition = dotMask `moveTo` (screenWidth - width `div` 2, screenHeight - height `div` 2)
          screenWidth = fst size
          screenHeight = snd size

moveTo :: SDL.Rect -> (CInt, CInt) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = x, rectY = y }


renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> SDL.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.renderCopy renderer texture

draw :: SDL.Renderer -> RenderInstructions -> IO ()
draw renderer instructions = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- instructions renderer
    SDL.renderPresent renderer

