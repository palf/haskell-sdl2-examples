{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.IO.Class
import           Control.Monad.Loops    (iterateUntilM)
import           Data.Foldable          (foldl')
import           Data.Text              hiding (foldl')
import           Data.Vector            ((!?))
import           Foreign.C.Types


loggerInfo :: (MonadIO m) => Text -> m ()
loggerInfo = liftIO . print


-- World definition

data World = World
  { exiting :: Bool
  , angle   :: Double
  }


initialWorld :: World
initialWorld = World
  { exiting = False
  , angle = 0
  }


setAngle :: Double -> World -> World
setAngle x w = w { angle = x }


quitWorld :: World -> World
quitWorld w = w { exiting = True }


-- Intents

data Intent
  = Idle
  | Quit
  | ChangeAngle Double


applyIntent :: Intent -> World -> World
applyIntent Idle            = id
applyIntent Quit            = quitWorld
applyIntent (ChangeAngle p) = setAngle p


updateWorld :: World -> [Intent] -> World
updateWorld = foldl' (flip applyIntent)


----------------------------------

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


openJoystick :: (MonadIO m) => m (Maybe SDL.Joystick)
openJoystick = do
  js <- SDL.availableJoysticks
  maybe (pure Nothing) (fmap Just . SDL.openJoystick) (js !? 0)


-- disableEventPolling :: (MonadIO m) => [Word32] -> m ()
-- disableEventPolling = mapM_ (`SDL.eventState` 0)


runUpdate :: (MonadIO m) => SDL.Joystick -> World -> m World
runUpdate g w = do
  es <- SDL.pollEvents
  let es' = payloadToIntent . SDL.eventPayload <$> es

  s <- getControllerState g
  let s' = mkTarget s

  liftIO $ print s
  pure $ updateWorld w (es' <> [ChangeAngle s'])

  where
    mkTarget ( a,  b)
      | safe a && safe b = 0
      | otherwise = (360 / (2 * pi) ) * atan2 b a

    safe x = -4096 < x && x < 4096


getControllerState :: (MonadIO m) => SDL.Joystick -> m (Double, Double)
getControllerState controller = do
  xValue <- SDL.axisPosition controller 0
  yValue <- SDL.axisPosition controller 1
  pure (fromIntegral xValue, fromIntegral yValue)



payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent = Quit
payloadToIntent _             = Idle


renderWorld :: (MonadIO m) => SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> m ()
renderWorld r tx w = do
  SDL.clear r
  drawWorld r tx w
  SDL.present r


drawWorld :: (MonadIO m) => SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> m ()
drawWorld r (t, ti) w
  = SDL.copyEx r t (Just mask) (Just pos) deg Nothing flips

  where
    tw :: Double
    tw = fromIntegral $ SDL.textureWidth ti
    th :: Double
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
