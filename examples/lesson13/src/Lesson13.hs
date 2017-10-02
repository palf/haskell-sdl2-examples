{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image
import qualified Common as C

import Control.Monad.Loops (iterateUntilM)
import Data.Foldable       (foldl')
import GHC.Word            (Word8)
import SDL                 (($=))


data World = World
  { exiting :: Bool
  , alpha   :: Word8
  }

data Intent
  = Increase
  | Decrease
  | Idle
  | Quit

data TextureMap a = TextureMap
  { background :: a
  , foreground :: a
  } deriving (Foldable, Traversable, Functor)


assetPaths :: TextureMap FilePath
assetPaths = TextureMap
  { background = "./assets/fadein.png"
  , foreground = "./assets/fadeout.png"
  }


initialWorld :: World
initialWorld = World
  { exiting = False
  , alpha = 0
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 13" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

      ts <- mapM (SDL.Image.loadTexture r) assetPaths
      mapM_ (\t -> SDL.textureBlendMode t $= SDL.BlendAlphaBlend) ts

      let doRender = renderWorld r ts

      _ <- iterateUntilM
        exiting
        (\x ->
          (updateWorld . foldl' runIntent x . mkIntent) <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

      mapM_ SDL.destroyTexture ts


mkIntent :: [SDL.Event] -> [Intent]
mkIntent = fmap (eventToIntent . extractPayload)


extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p


eventToIntent :: SDL.EventPayload -> Intent
eventToIntent SDL.QuitEvent         = Quit
eventToIntent (SDL.KeyboardEvent k) = keyEventToIntent k
eventToIntent _                     = Idle


keyEventToIntent :: SDL.KeyboardEventData -> Intent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeW      -> Increase
    SDL.KeycodeS      -> Decrease
    _                 -> Idle


runIntent :: World -> Intent -> World
runIntent w Increase  = increase w
runIntent w Decrease  = decrease w
runIntent w Idle      = id w
runIntent w Quit      = quit w


increase :: World -> World
increase w = w { alpha = alpha w + 8 }


decrease :: World -> World
decrease w = w { alpha = alpha w - 8 }


quit :: World -> World
quit w = w { exiting = True }


updateWorld :: World -> World
updateWorld = id


renderWorld :: SDL.Renderer -> TextureMap SDL.Texture -> World -> IO ()
renderWorld r ts w = do
  let fg = foreground ts
  let bg = background ts

  SDL.textureAlphaMod fg $= alpha w

  SDL.clear r
  SDL.copy r bg Nothing Nothing
  SDL.copy r fg Nothing Nothing
  SDL.present r
