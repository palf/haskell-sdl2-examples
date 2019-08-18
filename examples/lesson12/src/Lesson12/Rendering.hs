module Lesson12.Rendering
  ( renderWorld
  ) where

import           Lesson12.Types

import qualified SDL

import           Control.Monad.IO.Class (MonadIO)
import           SDL                    (($=))


renderWorld :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> World -> m ()
renderWorld r t w = do
  SDL.clear r
  modifyTexture t w
  SDL.copy r t Nothing Nothing
  SDL.present r


modifyTexture :: (MonadIO m) => SDL.Texture -> World -> m ()
modifyTexture t w = SDL.textureColorMod t $= rgb
  where
    convert v = floor . v . colors
    r = convert redV w
    g = convert greenV w
    b = convert blueV w
    rgb = SDL.V3 r g b
