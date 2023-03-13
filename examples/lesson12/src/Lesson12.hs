{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common              as C
import qualified Lesson12.Rendering  as R
import qualified Lesson12.World      as W
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Loops (iterateUntilM)
import           Lesson12.Intents


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle


updateWorld :: W.World -> [SDL.Event] -> W.World
updateWorld w
  = W.updateWorld w
  . fmap (payloadToIntent . SDL.eventPayload)


getKey :: SDL.KeyboardEventData -> Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeQ      -> Increase Red
    SDL.KeycodeW      -> Increase Green
    SDL.KeycodeE      -> Increase Blue
    SDL.KeycodeA      -> Decrease Red
    SDL.KeycodeS      -> Decrease Green
    SDL.KeycodeD      -> Decrease Blue
    SDL.KeycodeZ      -> Toggle Red
    SDL.KeycodeX      -> Toggle Green
    SDL.KeycodeC      -> Toggle Blue
    _                 -> Idle



main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 12" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      t <- SDL.Image.loadTexture r "./assets/colors.png"

      let doRender = R.renderWorld r t

      _ <- iterateUntilM
        W.exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        W.initialWorld

      SDL.destroyTexture t
