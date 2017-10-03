{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.Loops    (iterateUntilM)
import Data.Foldable          (foldl')


data Intent
  = Idle
  | Quit
  | Press Quadrant
  | Release Quadrant
  | Hover Quadrant
  | Leave Quadrant


data World = World
  { exiting :: Bool
  , panes   :: PaneMap
  }


data PaneMap = PaneMap
  { topLeft     :: Pane
  , topRight    :: Pane
  , bottomLeft  :: Pane
  , bottomRight :: Pane
  }


data Pane
  = Out
  | Over
  | Down
  | Up


data Quadrant
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight


initialWorld :: World
initialWorld = World
  { exiting = False
  , panes = initialPanes
  }


initialPanes :: PaneMap
initialPanes = PaneMap
  { topLeft     = Out
  , topRight    = Out
  , bottomLeft  = Out
  , bottomRight = Out
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 17" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/mouse_states.png"

      let doRender = renderWorld r t

      _ <- iterateUntilM
        exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

      SDL.destroyTexture (fst t)


updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle


motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = Hover q
  where
    q = selectQuadrant x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


  -- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Down
  --
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = t q
  where
    q = selectQuadrant x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
    t = if SDL.mouseButtonEventMotion e == SDL.Pressed
           then Press
           else Release


selectQuadrant :: (Num a, Ord a) => a -> a -> Quadrant
selectQuadrant x y
  | x <  320 && y <  240 = TopLeft
  | x >= 320 && y <  240 = TopRight
  | x <  320 && y >= 240 = BottomLeft
  | x >= 320 && y >= 240 = BottomRight
  | otherwise            = undefined


applyIntent :: Intent -> World -> World
applyIntent (Press q)   = pressWorld q
applyIntent (Release q) = releaseWorld q
applyIntent (Hover q)   = hoverWorld q
applyIntent (Leave q)   = leaveWorld q
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld


updatePaneMap :: (Pane -> Pane) -> (Pane -> Pane) -> Quadrant -> PaneMap -> PaneMap
updatePaneMap f g TopLeft     (PaneMap tl tr bl br) = PaneMap (f tl) (g tr) (g bl) (g br)
updatePaneMap f g TopRight    (PaneMap tl tr bl br) = PaneMap (g tl) (f tr) (g bl) (g br)
updatePaneMap f g BottomLeft  (PaneMap tl tr bl br) = PaneMap (g tl) (g tr) (f bl) (g br)
updatePaneMap f g BottomRight (PaneMap tl tr bl br) = PaneMap (g tl) (g tr) (g bl) (f br)


pressWorld :: Quadrant -> World -> World
pressWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setDown id q (panes w)


releaseWorld :: Quadrant -> World -> World
releaseWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setUp id q (panes w)


hoverWorld :: Quadrant -> World -> World
hoverWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setOver setOut q (panes w)


leaveWorld :: Quadrant -> World -> World
leaveWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setOut setOver q (panes w)


setOut :: Pane -> Pane
setOut Down = Down
setOut _ = Out


setOver :: Pane -> Pane
setOver Down = Down
setOver Up = Up
setOver _ = Over


setDown :: Pane -> Pane
setDown _ = Down


setUp :: Pane -> Pane
setUp Down = Up
setUp p = p


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }


renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
renderWorld r t w = do
  SDL.clear r
  drawWorld r t w
  SDL.present r


drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
drawWorld r (t, ti) w = do
  renderPane (topLeft     $ panes w) TopLeft
  renderPane (topRight    $ panes w) TopRight
  renderPane (bottomLeft  $ panes w) BottomLeft
  renderPane (bottomRight $ panes w) BottomRight

    where
      tw :: Double
      tw = fromIntegral $ SDL.textureWidth ti
      th = fromIntegral $ SDL.textureHeight ti

      s = C.mkRect 0 0 (tw / 2) (th / 2)

      mFor c = s `moveTo` (getMask c)
      pFor c = s `moveTo` (getPosition c)

      renderPane p q
        = SDL.copy r t
            (Just $ floor <$> mFor p)
            (Just $ floor <$> pFor q)


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
