{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common              as C
import qualified SDL

import           Control.Monad.Loops (iterateUntilM)
import           Data.Foldable       (foldl')


data Intent
  = Idle
  | Quit
  | Target CInt Cint


data World = World
  { exiting :: Bool
  , target  :: (CInt, CInt)
  }


initialWorld :: World
initialWorld = World
  { exiting = False
  , target = (0, 0)
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 19" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/arrow.png"

      g <- SDL.openGameController 0
      if isNothing g then fail "no controller found" else print "found controller"

      disableEventPolling [SDL.SDL_CONTROLLERAXISMOTION, SDL.SDL_JOYAXISMOTION]

      let doRender = renderWorld r t

      _ <- iterateUntilM
        exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

      SDL.closeGameController g
      SDL.destroyTexture (fst t)


disableEventPolling :: [Word32] -> IO ()
disableEventPolling = mapM_ (`SDL.eventState` 0)


updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent _                        = Idle


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }


renderWorld :: SDL.Renderer -> SDL.Texture -> World -> IO ()
renderWorld r t w = do
  SDL.clear r
  drawWorld r t w
  SDL.present r


drawWorld :: SDL.Renderer -> SDL.Texture -> World -> IO ()
drawWorld r tex w
  = SDL.copyEx r tex mask pos degrees Nothing flip

    where
      w = 89
      h = 50
      sprite = SDL.mkRect 1 0 w h
      mask = Just sprite
      pos = Just (sprite `moveBy` superScale x)
      x = 1
      degrees = 0
      flips = SDL.V2 False False



getMask :: (Num a) => Pane -> (a, a)
getMask Out  = (  0,   0)
getMask Over = (320,   0)
getMask Down = (  0, 240)
getMask Up   = (320, 240)


getPosition :: (Num a) => Quadrant -> (a, a)
getPosition TopLeft     = (  0,   0)
getPosition TopRight    = (320,   0)
getPosition BottomLeft  = (  0, 240)
getPosition BottomRight = (320, 240)


moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (C.mkPoint x y) d


superScale :: (Double, Double) -> (Int, Int)
superScale = pairMap $ floor . (*) 200


pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)


getControllerState :: Lib.GameController -> IO (Double, Double)
getControllerState controller = do
    xValue <- getAxisState controller 0
    yValue <- getAxisState controller 1
    let range = hypotenuse xValue yValue
    let deadZone = 8000 ^ (2 :: Integer)
    let carpetValue = if range < deadZone
        then (0, 0)
        else pairMap ssscale (xValue, yValue)
    pure carpetValue
      where ssscale x = fromIntegral x / 32768


hypotenuse :: (Num a) => a -> a -> a
hypotenuse a b = a ^ two + b ^ two
  where two = 2 :: Integer


getAxisState :: Lib.GameController -> Lib.GameControllerAxis -> IO Int
getAxisState controller index = do
    axis <- Lib.gameControllerGetAxis controller index
    pure $ fromIntegral axis
