module Shared.Input (
    Input,
    handleNoInput,

    KeyDirection  (..),
    Key (..),
    handleKeyInput,
    getKey
) where

import qualified Graphics.UI.SDL as SDL
import Shared.Input.Keys

type Input = Maybe SDL.Event

handleNoInput :: IO (Maybe SDL.Event) -> IO Bool
handleNoInput stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False
        Just (SDL.QuitEvent _ _) -> return True
        _ -> return False

