{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.Extra    (whileM)


data Intent
  = SelectSurface KeyPress
  | Idle
  | Quit


data KeyPress
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | Unknown


surfacePaths :: [FilePath]
surfacePaths =
  [ "./assets/press.bmp"
  , "./assets/up.bmp"
  , "./assets/down.bmp"
  , "./assets/left.bmp"
  , "./assets/right.bmp"
  ]


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 04" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    surfaces <- mapM SDL.loadBMP surfacePaths

    let doRender = C.renderSurfaceToWindow w screen
    doRender (head surfaces)

    whileM $
      mkIntent <$> SDL.pollEvent
      >>= runIntent surfaces doRender

    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface screen


mkIntent :: Maybe SDL.Event -> Intent
mkIntent Nothing = Idle
mkIntent (Just e) = eventToIntent (extractPayload e)


extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p


eventToIntent :: SDL.EventPayload -> Intent
eventToIntent SDL.QuitEvent         = Quit
eventToIntent (SDL.KeyboardEvent k) = SelectSurface (getKey k)
eventToIntent _                     = Idle


getKey :: SDL.KeyboardEventData -> KeyPress
getKey (SDL.KeyboardEventData _ _ _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeRight -> KeyRight
    SDL.KeycodeLeft  -> KeyLeft
    SDL.KeycodeDown  -> KeyDown
    SDL.KeycodeUp    -> KeyUp
    _                -> Unknown


runIntent :: (Monad m) => [a] -> (a -> m ()) -> Intent -> m Bool
runIntent _ _ Quit
  = pure False

runIntent _ _ Idle
  = pure True

runIntent cs f (SelectSurface key)
  = const True <$> f (selectSurface cs key)


selectSurface :: [a] -> KeyPress -> a
selectSurface xs k = xs !! (selectSurfaceIndex k)


selectSurfaceIndex :: KeyPress -> Int
selectSurfaceIndex KeyUp    = 1
selectSurfaceIndex KeyDown  = 2
selectSurfaceIndex KeyLeft  = 3
selectSurfaceIndex KeyRight = 4
selectSurfaceIndex _        = 0
