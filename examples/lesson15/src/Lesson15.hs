{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Loops    (iterateUntilM)
import           Data.Foldable          (foldl')


data FlipDirection
  = Horizontal
  | Vertical


data RotateDirection
  = Clock
  | Counter


data Intent
  = Idle
  | Flip FlipDirection
  | Rotate RotateDirection
  | Reset
  | Quit


data World = World
  { exiting :: Bool
  , degrees :: Int
  , flipped :: (Bool, Bool)
  }


initialWorld :: World
initialWorld = World
  { exiting = False
  , degrees = 0
  , flipped = (False, False)
  }


keyEventToIntent :: SDL.KeyboardEventData -> Intent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of

    SDL.KeycodeEscape -> Quit

    SDL.KeycodeQ      -> Rotate Counter
    SDL.KeycodeE      -> Rotate Clock

    SDL.KeycodeA      -> Flip Horizontal
    SDL.KeycodeD      -> Flip Vertical

    SDL.KeycodeR      -> Reset
    _                 -> Idle

keyEventToIntent _ = Idle


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = keyEventToIntent k
payloadToIntent _                     = Idle


flipWorld :: FlipDirection -> World -> World
flipWorld Horizontal w = w { flipped = (h', v') }
  where h' = not ( fst (flipped w) )
        v' = snd (flipped w)

flipWorld Vertical w = w { flipped = (h', v') }
  where h' = fst (flipped w)
        v' = not (snd (flipped w))


rotateWorld :: RotateDirection -> World -> World
rotateWorld Clock w   = w { degrees = degrees w + 15 }
rotateWorld Counter w = w { degrees = degrees w - 15 }


resetWorld :: World -> World
resetWorld _ = initialWorld


quitWorld :: World -> World
quitWorld w = w { exiting = True }


applyIntent :: Intent -> World -> World
applyIntent (Flip d)   = flipWorld d
applyIntent (Rotate d) = rotateWorld d
applyIntent Reset      = resetWorld
applyIntent Idle       = id
applyIntent Quit       = quitWorld


updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)


renderWorld
  :: (MonadIO m)
  => SDL.Renderer
  -> (SDL.Texture, SDL.TextureInfo)
  -> World
  -> m ()

renderWorld r (t, ti) w = do
  SDL.clear r
  SDL.copyEx r t (Just mask) (Just pos) deg Nothing flips
  SDL.present r

  where
    tw :: Double
    th :: Double
    tw = fromIntegral $ SDL.textureWidth ti
    th = fromIntegral $ SDL.textureHeight ti

    s :: SDL.Rectangle Double
    s = C.mkRect 0 0 640 480
    box = C.mkRect 0 0 tw th

    mask = floor <$> s
    pos = round <$> C.centerWithin box s

    deg = fromIntegral $ degrees w
    flips = uncurry SDL.V2 (flipped w)


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 15" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      tx <- C.loadTextureWithInfo r "./assets/arrow.png"

      let doRender = renderWorld r tx

      _ <- iterateUntilM
        exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

      SDL.destroyTexture (fst tx)
