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

initialApplication :: Application
initialApplication = Application { exiting = False, gameworld = initialWorld }

initialWorld :: World
initialWorld = World { red = 128, green = 128, blue = 128 }

main :: IO ()
main = inWindow $ \window -> Image.withImgInit [Image.InitPNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    withAssets renderer ["./assets/colors.png"] $ runApplication renderer
    SDL.destroyRenderer renderer

data Application = Application { exiting :: Bool, gameworld :: World } deriving (Show)
data World = World { red :: Word8, green :: Word8, blue :: Word8 } deriving (Show)
data Colour = Red | Green | Blue deriving (Show, Eq)
data Intent = Increase Colour | Decrease Colour | DoNothing | Quit deriving (Show, Eq)
type UpdateApplication = Application -> Application

runApplication :: SDL.Renderer -> [Asset] -> IO ()
runApplication renderer assets = repeatUntilGameover applyAndDraw initialApplication
    where applyIntents = updateSource collectEvents
          drawApplication = draw renderer assets
          applyAndDraw = runstuff (applyIntents, drawApplication)

repeatUntilGameover :: (Monad m) => (Application -> m Application) -> Application -> m ()
repeatUntilGameover t = go
  where go application = t application >>= efunc
        efunc application = unless (exiting application) (go application)

runstuff :: (Monad m) => (m UpdateApplication, Application -> m ()) -> Application -> m Application
runstuff (updateFunc, drawFunc) application = updateFunc <*> pure application >>= \application' ->
          drawFunc application' >> return application'

collectIntents :: (Monad m, Functor f) => m (f Event) -> m (f Intent)
collectIntents = liftM (fmap eventToIntent)

updateSource :: (Monad m, Functor f, Foldable f) => m (f Event) -> m UpdateApplication
updateSource events = flip stepApplication <$> collectIntents events

stepApplication :: (Foldable f) => Application -> f Intent -> Application
stepApplication = foldl (flip applyIntent)

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

applyIntent :: Intent -> UpdateApplication
applyIntent (Increase color) = increase color
applyIntent (Decrease color) = decrease color
applyIntent DoNothing = id
applyIntent Quit = quit

type Identity a = a -> a
data Lens a b = Lens { getL :: a -> b, setL :: b -> a -> a }
type ColourLens = Lens World Word8

modL :: Lens a b -> a -> (b -> b) -> a
modL lens record func = setL lens newValue record
  where value = getL lens record
        newValue = func value

redLens :: ColourLens
redLens = Lens red setRed
  where setRed x world = world { red = x }

greenLens :: ColourLens
greenLens = Lens green setGreen
    where setGreen x world = world { green = x }

blueLens :: ColourLens
blueLens = Lens blue setBlue
    where setBlue x world = world { blue = x }

increase :: Colour -> UpdateApplication
increase Red = modifyColour redLens (+ 16)
increase Green = modifyColour greenLens (+ 16)
increase Blue = modifyColour blueLens (+ 16)

decrease :: Colour -> UpdateApplication
decrease Red = modifyColour redLens (flip (-) 16)
decrease Green = modifyColour greenLens (flip (-) 16)
decrease Blue = modifyColour blueLens (flip (-) 16)

quit :: UpdateApplication
quit application = application { exiting = True }

modifyColour :: ColourLens -> Identity Word8 -> UpdateApplication
modifyColour lens func application = application { gameworld = world' }
  where world' = modL lens world func
        world = gameworld application

draw :: SDL.Renderer -> [Asset] -> Application -> IO ()
draw renderer assets application = do
    let instructions = writeRenderInstructions assets application
    withBlankScreen renderer $ executeRender renderer instructions

data RenderInstruction =
    SetTextureColor SDL.Texture Word8 Word8 Word8 |
    RenderCopy SDL.Texture SDL.Rect

writeRenderInstructions :: [Asset] -> Application -> [RenderInstruction]
writeRenderInstructions assets (Application _ world) = [
    SetTextureColor texture r g b ,
    RenderCopy texture position ]
    where World r b g = world
          (texture, width, height) = head assets
          position = SDL.Rect { rectX = 0, rectY = 0, rectW = width, rectH = height }

executeRender :: (Foldable f) => SDL.Renderer -> f RenderInstruction -> IO ()
executeRender renderer = mapM_ (executeInstruction renderer)

executeInstruction :: SDL.Renderer -> RenderInstruction -> IO ()
executeInstruction _ (SetTextureColor texture r g b) = void $ SDL.setTextureColorMod texture r g b
executeInstruction renderer (RenderCopy texture position) = void $ with position $ SDL.renderCopy renderer texture nullPtr

