{-# LANGUAGE DeriveFoldable    #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
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
  = SelectSurface Direction
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


selectSurface :: Direction -> AssetMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right


readEventIntents :: (MonadIO m) => m (Maybe Intent)
readEventIntents = firstJust payloadToIntent . fmap SDL.eventPayload <$> SDL.pollEvents
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent SDL.QuitEvent = Just Quit
    payloadToIntent _             = Nothing


applyIntent :: (Monad m) => AssetMap a -> RenderFunction m a -> Intent -> m ()
applyIntent _ _ Quit = pure ()
applyIntent cs render (SelectSurface key)
  = render (selectSurface key cs)


readKeyboardIntents :: (MonadIO m) => m [Intent]
readKeyboardIntents = do
  checkKey <- SDL.getKeyboardState

  let scans =
        [ ( SDL.ScancodeUp,  SelectSurface Up )
        , ( SDL.ScancodeDown,  SelectSurface Down )
        , ( SDL.ScancodeLeft,  SelectSurface Left )
        , ( SDL.ScancodeRight,  SelectSurface Right )
        ]

  let ss = filter (\(s, _) -> checkKey s) scans  :: [( SDL.Scancode, Intent )]

  pure $ case ss of
           [] -> [SelectSurface Help]
           ts -> snd <$> ts


appLoop :: (MonadIO m) => AssetMap a -> RenderFunction m a -> m Bool
appLoop assets doRender = do
  xs <- readEventIntents

  case xs of
    Just Quit -> pure False

    _ -> do

      ks <- readKeyboardIntents
      applyIntent assets doRender `mapM_` ks
      pure True


main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 18" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    assets <- mapM SDL.loadBMP surfacePaths

    let doRender = C.renderSurfaceToWindow w screen

    doRender (help assets)
    whileM $ appLoop assets doRender

    mapM_ SDL.freeSurface assets
    SDL.freeSurface screen
