{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.Extra    (whileM)
import Prelude hiding (Left, Right)

data Intent
  = SelectSurface Direction
  | Idle
  | Quit


data Direction
  = Help
  | Up
  | Down
  | Left
  | Right


data SurfaceMap a = SurfaceMap
  { help  :: a
  , up    :: a
  , down  :: a
  , left  :: a
  , right :: a
  } deriving (Foldable, Traversable, Functor)


surfacePaths :: SurfaceMap FilePath
surfacePaths = SurfaceMap
  { help  = "./assets/press.bmp"
  , up    = "./assets/up.bmp"
  , down  = "./assets/down.bmp"
  , left  = "./assets/left.bmp"
  , right = "./assets/right.bmp"
  }


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 04" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    surfaces <- mapM SDL.loadBMP surfacePaths

    let doRender = C.renderSurfaceToWindow w screen
    doRender (help surfaces)

    whileM $
      mkIntent <$> SDL.pollEvent
      >>= runIntent surfaces doRender

    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface screen


mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (eventToIntent . extractPayload)


extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p


eventToIntent :: SDL.EventPayload -> Intent
eventToIntent SDL.QuitEvent         = Quit
eventToIntent (SDL.KeyboardEvent k) = getKey k
eventToIntent _                     = Idle


getKey :: SDL.KeyboardEventData -> Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> SelectSurface Up
    SDL.KeycodeDown   -> SelectSurface Down
    SDL.KeycodeLeft   -> SelectSurface Left
    SDL.KeycodeRight  -> SelectSurface Right
    _                 -> SelectSurface Help


runIntent :: (Monad m) => SurfaceMap a -> (a -> m ()) -> Intent -> m Bool
runIntent _ _ Quit
  = pure False

runIntent _ _ Idle
  = pure True

runIntent cs f (SelectSurface key)
  = True <$ f (selectSurface key cs)


selectSurface :: Direction -> SurfaceMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right
