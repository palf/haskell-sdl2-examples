{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lesson12.Types
import qualified Lesson12.Transitions as T
import qualified Lesson12.Rendering as R

import qualified SDL
import qualified SDL.Image
import qualified Common as C

import Control.Monad.Loops (iterateUntilM)
import Data.Foldable (foldl')


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
          (T.updateWorld . foldl' runIntent x . mkIntent) <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

      SDL.destroyTexture t



mkIntent :: [SDL.Event] -> [Intent]
mkIntent = fmap (eventToIntent . extractPayload)


extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p


eventToIntent :: SDL.EventPayload -> Intent
eventToIntent SDL.QuitEvent         = Quit
eventToIntent (SDL.KeyboardEvent k) = getKey k
eventToIntent _                     = Idle


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


runIntent :: World -> Intent -> World
runIntent w (Increase color) = T.increase color w
runIntent w (Decrease color) = T.decrease color w
runIntent w (Toggle color)   = T.toggle color w
runIntent w Idle             = w
runIntent w Quit             = T.exit w
