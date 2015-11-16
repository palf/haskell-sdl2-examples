module Shared.Input (
    Input,
    Inputs,
    pollForQuit,

    KeyPress (..),
    getKey
) where

import qualified Graphics.UI.SDL as SDL
import Shared.Input.Keys

type Input = Maybe SDL.Event
type Inputs = [SDL.Event]

pollForQuit :: IO (Maybe SDL.Event) -> IO Bool
pollForQuit stream = do
    maybeEvent <- stream
    case maybeEvent of
        Just (SDL.QuitEvent _ _) -> return True
        _ -> return False

