{-# LANGUAGE DeriveFoldable    #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe
import           Prelude                hiding (Left, Right)


data Direction
  = Help
  | Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)


data Intent
  = Render Direction
  | Quit
  deriving (Show, Eq)


data AssetMap a = AssetMap
  { help  :: a
  , up    :: a
  , down  :: a
  , left  :: a
  , right :: a
  } deriving (Foldable, Traversable, Functor)


type RenderFunction m = (Direction -> m ())


surfacePaths :: AssetMap FilePath
surfacePaths = AssetMap
  { help  = "./assets/press.bmp"
  , up    = "./assets/up.bmp"
  , down  = "./assets/down.bmp"
  , left  = "./assets/left.bmp"
  , right = "./assets/right.bmp"
  }


getKey :: SDL.KeyboardEventData -> Maybe Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) = Just $
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> Render Up
    SDL.KeycodeDown   -> Render Down
    SDL.KeycodeLeft   -> Render Left
    SDL.KeycodeRight  -> Render Right
    _                 -> Render Help


mapEventsToIntents :: [SDL.Event] -> [Intent]
mapEventsToIntents = mapMaybe (payloadToIntent . SDL.eventPayload)
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent (SDL.KeyboardEvent k) = getKey k
    payloadToIntent SDL.QuitEvent         = Just Quit
    payloadToIntent _                     = Nothing


appLoop :: (MonadIO m) => RenderFunction m -> m Bool
appLoop render = do
  xs <- mapEventsToIntents <$> SDL.pollEvents

  let shouldQuit = Quit `elem` xs
  if shouldQuit
      then pure False
      else do
        applyIntent `mapM_` xs
        pure True

  where
    applyIntent (Render key) = render key
    applyIntent Quit                = pure ()


draw :: (MonadIO m) => SDL.Window -> SDL.Surface -> AssetMap SDL.Surface -> RenderFunction m
draw w screen assets d = do
  let x = selectSurface d assets
  C.renderSurfaceToWindow w screen x

  where
    selectSurface :: Direction -> AssetMap a -> a
    selectSurface Help  = help
    selectSurface Up    = up
    selectSurface Down  = down
    selectSurface Left  = left
    selectSurface Right = right


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 04" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    assets <- mapM SDL.loadBMP surfacePaths

    let doRender = draw w screen assets

    doRender Help
    whileM $ appLoop doRender

    mapM_ SDL.freeSurface assets
    SDL.freeSurface screen
