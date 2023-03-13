module Common where

import qualified SDL
import qualified SDL.Image

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)

import           SDL                    (($=))


-- NOTE: can throw
withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initializeAll
  _ <- op
  SDL.quit


-- NOTE: probably not required
withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op
  = SDL.Image.initialize [] >> void op >> SDL.Image.quit


withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral x) (fromIntegral y)


withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r


rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i
  = SDL.surfaceBlit i Nothing s Nothing
  >> SDL.updateWindowSurface w


hasQuitEvent :: [SDL.Event] -> Bool
hasQuitEvent = elem SDL.QuitEvent . map SDL.eventPayload


setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest


loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)


mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h


moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (mkPoint x y) d


centerWithin :: (Fractional a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
  = SDL.Rectangle p iz
  where
    p = SDL.P $ op + (oz - iz) / 2

