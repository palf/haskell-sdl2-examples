module Shared.Polling where

import qualified Graphics.UI.SDL as SDL
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable


repeatUntil :: IO Bool -> IO a -> IO ()
repeatUntil quitClause operation = do
    operation
    isQuitting <- quitClause
    unless isQuitting $ repeatUntil quitClause operation

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

