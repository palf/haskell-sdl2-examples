{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common              as C
import qualified SDL

import Foreign.C.Types
import Control.Monad.IO.Class
import Data.Text hiding (foldl')
import Data.Vector ((!?))
import           Control.Monad.Loops (iterateUntilM)
import           Data.Foldable       (foldl')


loggerInfo :: (MonadIO m) => Text -> m ()
loggerInfo = liftIO . print


data Intent
  = Idle
  | Quit
  | ChangeAngle Double


data World = World
  { exiting :: Bool
  , angle  :: Double
  }


initialWorld :: World
initialWorld = World
  { exiting = False
  , angle = 0
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 19" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      tx <- C.loadTextureWithInfo r "./assets/arrow.png"

      g <- openJoystick

      case g of
        Nothing ->
          loggerInfo "no controller found"

        Just g' ->  do
          loggerInfo "found controller"

          -- disableEventPolling [SDL.SDL_CONTROLLERAXISMOTION, SDL.SDL_JOYAXISMOTION]

          let doRender = renderWorld r tx

          _ <- iterateUntilM
            exiting
            (sideEffect (runUpdate g') doRender)
            initialWorld

          SDL.closeJoystick g'
      SDL.destroyTexture (fst tx)


openJoystick :: IO (Maybe SDL.Joystick)
openJoystick = do
  js <- SDL.availableJoysticks
  maybe (pure Nothing) (fmap Just . SDL.openJoystick) (js !? 0)


-- disableEventPolling :: [Word32] -> IO ()
-- disableEventPolling = mapM_ (`SDL.eventState` 0)


runUpdate :: SDL.Joystick -> World -> IO World
runUpdate g w = do
  es <- SDL.pollEvents
  let es' = fmap (payloadToIntent . SDL.eventPayload) es
  s <- getControllerState g
  let s' = mkTarget s
  liftIO $ print s

  pure $ updateWorld w (es' <> [ChangeAngle s'])

  where
    mkTarget ( a,  b)
      | safe a && safe b = 0
      | otherwise = (360 / (2 * pi) ) * atan2 b a

    safe x = -4096 < x && x < 4096


getControllerState :: SDL.Joystick -> IO (Double, Double)
getControllerState controller = do
  xValue <- SDL.axisPosition controller 0
  yValue <- SDL.axisPosition controller 1
  pure (fromIntegral xValue, fromIntegral yValue)


updateWorld :: World -> [Intent] -> World
updateWorld = foldl' (flip applyIntent)


applyIntent :: Intent -> World -> World
applyIntent Quit = quitWorld
applyIntent Idle = idleWorld
applyIntent (ChangeAngle p) = targetPoint p


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent _                        = Idle


idleWorld :: World -> World
idleWorld = id


targetPoint :: Double -> World -> World
targetPoint x w = w { angle = x }


quitWorld :: World -> World
quitWorld w = w { exiting = True }


renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
renderWorld r tx w = do
  SDL.clear r
  drawWorld r tx w
  SDL.present r


drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
drawWorld r (t, ti) w
  = SDL.copyEx r t (Just mask) (Just pos) deg Nothing flips

  where
    tw :: Double
    th :: Double
    tw = fromIntegral $ SDL.textureWidth ti
    th = fromIntegral $ SDL.textureHeight ti

    s :: SDL.Rectangle Double
    s = C.mkRect 0 0 640 480
    box = C.mkRect 0 0 tw th

    mask = floor <$> s
    pos = floor <$> centerWithin box s

    deg = CDouble $ angle w
    flips = SDL.V2 False False


sideEffect :: (Monad m) => (a -> m b) -> (b -> m ()) -> a -> m b
sideEffect op ef x = do
  x' <- op x
  ef x'
  pure x'



centerWithin :: (Fractional a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
  = SDL.Rectangle p iz

  where
    p = SDL.P $ op + (oz - iz) / 2
