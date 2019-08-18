{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Lesson12.Rendering   as R
import qualified Lesson12.Transitions as T
import           Lesson12.Types

import qualified Common               as C
import qualified SDL
import qualified SDL.Image

import           Control.Monad.Loops  (iterateUntilM)
import           Data.Foldable        (foldl')


initialWorld :: World
initialWorld = World
  { colors = ColorValues
    { redV   = 128
    , greenV = 128
    , blueV  = 128
    }
  , toggles = ColorToggles
    { redT   = False
    , greenT = False
    , blueT  = False
    }
  , exiting = False
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 12" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      t <- SDL.Image.loadTexture r "./assets/colors.png"

      let doRender = R.renderWorld r t

      _ <- iterateUntilM
        exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

      SDL.destroyTexture t



updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = T.stepWorld
  . foldl' (flip runIntent) w
  . mkIntent


mkIntent :: [SDL.Event] -> [Intent]
mkIntent = fmap (payloadToIntent . SDL.eventPayload)


payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle


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


runIntent :: Intent -> World -> World
runIntent (Increase color) = T.increase color
runIntent (Decrease color) = T.decrease color
runIntent (Toggle color)   = T.toggle color
runIntent Idle             = id
runIntent Quit             = T.exit
