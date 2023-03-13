{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common              as C
import qualified SDL

import           Control.Monad.Extra (whileM)
import           Prelude             hiding (Left, Right)

data Intent
  = SelectSurface Direction
  | Idle
  | Quit
  deriving (Show, Eq)


data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)


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

    whileM $ do
      xs <- mkIntent <$> SDL.pollEvents
      _ <- applyIntent surfaces doRender `mapM` xs
      pure $ notElem Quit xs

    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface screen


mkIntent :: [SDL.Event] -> [ Intent ]
mkIntent = fmap (payloadToIntent . SDL.eventPayload)


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle


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


applyIntent :: (Monad m) => SurfaceMap a -> (a -> m ()) -> Intent -> m ()
applyIntent _ _ Quit
  = pure ()

applyIntent _ _ Idle
  = pure ()

applyIntent cs f (SelectSurface key)
  = f (selectSurface key cs)


selectSurface :: Direction -> SurfaceMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right
