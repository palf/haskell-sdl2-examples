module Shared.Polling where

import qualified Graphics.UI.SDL as SDL
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable


repeatUntil :: IO Bool -> IO a -> IO ()
repeatUntil quitClause operation = do
    _ <- operation
    isQuitting <- quitClause
    unless isQuitting $ repeatUntil quitClause operation

repeatUntilTrue :: IO Bool -> IO ()
repeatUntilTrue operation = do
    isTrue <- operation
    unless isTrue $ repeatUntilTrue operation

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
    event <- pollEvent
    case event of
      Nothing -> return []
      Just thing -> do
          print thing
          events <- collectEvents
          return $ thing : events

