module Shared.Input.Keys (
    KeyDirection (..),
    Key (..),
    handleKeyInput,
    getKey
) where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types

data KeyDirection = KeyUp | KeyDown | KeyLeft | KeyRight | KeyOther deriving (Show, Read)
data Key = Q | W | E | A | S | D | Unknown

handleKeyInput :: IO (Maybe SDL.Event) -> (KeyDirection -> IO a) -> IO Bool
handleKeyInput stream keyHandler = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False
        Just (SDL.QuitEvent _ _) -> return True
        Just (SDL.KeyboardEvent _ _ _ _ _ keysym) -> do
            _ <- keyHandler $ keymap keysym
            return False
        _ -> return False

keymap :: SDL.Keysym -> KeyDirection
keymap (SDL.Keysym code _ _) = case code of
    79 -> KeyRight
    80 -> KeyLeft
    81 -> KeyDown
    82 -> KeyUp
    _ -> KeyOther

getKey :: SDL.Keysym -> Key
getKey keysym = case keysymScancode keysym of
    4  -> A
    7  -> D
    8  -> E
    20 -> Q
    22 -> S
    26 -> W
    _  -> Unknown

