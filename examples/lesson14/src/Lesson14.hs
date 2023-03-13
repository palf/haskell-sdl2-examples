{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Image
import qualified SDL.Raw.Timer          as Raw

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List.Extra


data World = World
  { exiting :: Bool
  , frame   :: Int
  } deriving (Show)


data Intent = Quit deriving (Show)


initialApp :: World
initialApp = World
  { exiting = False
  , frame   = 0
  }


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = loop
  where loop a = f a >>= \b -> unless (p b) (loop b)


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


-- TODO: this should be in common
measureFPS :: (MonadIO m) => m a -> m a
measureFPS op = do
    start <- Raw.getPerformanceCounter
    x <- op
    end <- Raw.getPerformanceCounter

    freq <- Raw.getPerformanceFrequency
    let elapsed = (fromIntegral (end - start) / fromIntegral freq) :: Double
    liftIO $ print (1 / elapsed)
    pure x


pollIntents :: (MonadIO m) => m (Maybe Intent)
pollIntents = firstJust f . fmap SDL.eventPayload <$> SDL.pollEvents
  where
    f :: SDL.EventPayload -> Maybe Intent
    f SDL.QuitEvent = Just Quit
    f _             = Nothing


updateApp :: World -> Maybe Intent -> World
updateApp a Nothing     = a { frame = frame a + 1 }
updateApp a (Just Quit) = a { exiting = True }


appLoop :: (MonadIO m) => (World -> m ()) -> World -> m World
appLoop r w = do
  xs <- pollIntents
  let w' = updateApp w xs
  r w'
  pure w'


renderApp :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> World -> m ()
renderApp r t a = do
  SDL.clear r
  SDL.copy r t (Just mask) (Just pos)
  SDL.present r

  where
    framesPerSecond = 60 :: Double
    animDurationSeconds = 0.8 :: Double
    animFrames = 8 :: Int

    -- ax :: seconds for one frame of animation
    -- fx :: number of seconds total
    ax = fromIntegral animFrames / animDurationSeconds
    fx = fromIntegral (frame a) / framesPerSecond

    x = floor (ax * fx) `mod` animFrames

    mask = C.mkRect (fromIntegral x * 48) 0 48 48
    s = C.mkRect 0 0 192 (192 :: Double)
    w = C.mkRect 0 0 640 480
    pos = round <$> C.centerWithin s w


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 14" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      t <- SDL.Image.loadTexture r "./assets/walk.png"
      let doRender = renderApp r t
      runApp (appLoop doRender) initialApp
      SDL.destroyTexture t
