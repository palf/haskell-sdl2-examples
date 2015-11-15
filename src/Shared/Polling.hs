module Shared.Polling where

import qualified Graphics.UI.SDL as SDL
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

-- TODO: similar to Control.Monad.Loops, maybe replace?
repeatUntil :: (Monad m) => m Bool -> m a -> m ()
repeatUntil p f = go
    where go = f >> p >>= flip unless go

repeatUntilTrue :: (Monad m) => m Bool -> m ()
repeatUntilTrue p = go
  where go = p >>= flip unless go

sdlQuit :: IO Bool
sdlQuit = liftM isQuitEvent pollEvent

isQuitEvent :: Maybe SDL.Event -> Bool
isQuitEvent Nothing = False
isQuitEvent (Just event) = case event of
    SDL.QuitEvent _ _ -> True
    _ -> False

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pointer -> do
    status <- SDL.pollEvent pointer
    if status == 1
       then maybePeek peek pointer
       else return Nothing

collectEvents :: IO [SDL.Event]
collectEvents = do
    maybeEvent <- pollEvent
    case maybeEvent of
      Nothing -> return []
      Just event -> do
          events <- collectEvents
          return $ event : events

