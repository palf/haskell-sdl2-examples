{-# LANGUAGE DeriveFoldable    #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Prelude                hiding (Left, Right)


data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)


data Intent
  = SelectSurface Direction
  | Idle
  | Quit
  deriving (Show, Eq)


data AssetMap a = AssetMap
  { help  :: a
  , up    :: a
  , down  :: a
  , left  :: a
  , right :: a
  } deriving (Foldable, Traversable, Functor)


type RenderFunction m a = (a -> m ())


surfacePaths :: AssetMap FilePath
surfacePaths = AssetMap
  { help  = "./assets/press.bmp"
  , up    = "./assets/up.bmp"
  , down  = "./assets/down.bmp"
  , left  = "./assets/left.bmp"
  , right = "./assets/right.bmp"
  }


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


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle


selectSurface :: Direction -> AssetMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right


applyIntent :: (Monad m) => AssetMap a -> RenderFunction m a -> Intent -> m ()
applyIntent _ _ Quit = pure ()
applyIntent _ _ Idle = pure ()
applyIntent cs f (SelectSurface key)
  = f (selectSurface key cs)


mapEventsToIntents :: [SDL.Event] -> [ Intent ]
mapEventsToIntents = fmap (payloadToIntent . SDL.eventPayload)


appLoop :: (MonadIO m) => AssetMap a -> RenderFunction m a -> m Bool
appLoop assets doRender = do
  xs <- mapEventsToIntents <$> SDL.pollEvents

  let shouldQuit = Quit `elem` xs
  if shouldQuit
      then pure False
      else do
        applyIntent assets doRender `mapM_` xs
        pure True


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 04" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    assets <- mapM SDL.loadBMP surfacePaths

    let doRender = C.renderSurfaceToWindow w screen

    doRender (help assets)
    whileM $ appLoop assets doRender

    mapM_ SDL.freeSurface assets
    SDL.freeSurface screen
