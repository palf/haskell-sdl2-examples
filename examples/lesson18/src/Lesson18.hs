{-# LANGUAGE DeriveFoldable    #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor
import           Data.List.Extra
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


readEventIntents :: (MonadIO m) => m (Maybe Intent)
readEventIntents = firstJust payloadToIntent . fmap SDL.eventPayload <$> SDL.pollEvents
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent SDL.QuitEvent = Just Quit
    payloadToIntent _             = Nothing


readKeyboardIntents :: (MonadIO m) => m Intent
readKeyboardIntents = mapScansToIntents <$> SDL.getKeyboardState
  where
    scans =
        [ ( SDL.ScancodeEscape, Quit )
        , ( SDL.ScancodeUp, Render Up )
        , ( SDL.ScancodeDown, Render Down )
        , ( SDL.ScancodeLeft, Render Left )
        , ( SDL.ScancodeRight, Render Right )
        ]

    mapScansToIntents checkKey =
      case filter (checkKey . fst) scans of
        [] -> Render Help
        ts -> head $ snd <$> ts


appLoop :: (MonadIO m) => RenderFunction m -> m Bool
appLoop render = do
  xs <- readEventIntents

  case xs of
    Just Quit ->
      pure False

    _ -> do
      ks <- readKeyboardIntents
      applyIntent ks

  where
    applyIntent (Render key) = render key $> True
    applyIntent Quit         = pure False


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
main = C.withSDL $ C.withWindow "Lesson 18" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    assets <- mapM SDL.loadBMP surfacePaths

    let doRender = draw w screen assets

    doRender Help
    whileM $ appLoop doRender

    mapM_ SDL.freeSurface assets
    SDL.freeSurface screen
