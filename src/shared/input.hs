module Shared.Input where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types

data KeyDirection = KeyUp | KeyDown | KeyLeft | KeyRight | KeyOther deriving (Show, Read)
data Key = Q | W | E | A | S | D | Unknown

handleNoInput :: IO (Maybe SDL.Event) -> IO Bool
handleNoInput stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False
        Just (SDL.QuitEvent _ _) -> return True
        _ -> return False

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
    20 -> Q
    26 -> W
    8  -> E
    4  -> A
    22 -> S
    7  -> D
    _  -> Unknown

