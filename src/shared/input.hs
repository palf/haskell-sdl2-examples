module Shared.Input where

import qualified Graphics.UI.SDL as SDL

data KeyDirection = KeyUp | KeyDown | KeyLeft | KeyRight | KeyOther deriving (Show, Read)

handle :: IO (Maybe SDL.Event) -> (KeyDirection -> IO a) -> IO Bool
handle stream keyHandler = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False
        Just (SDL.QuitEvent _ _) -> return True
        Just (SDL.KeyboardEvent _ _ _ _ _ keysym) -> do
            keyHandler $ keymap keysym
            return False
        _ -> return False

keymap :: SDL.Keysym -> KeyDirection
keymap (SDL.Keysym keysymScancode _ _) = case keysymScancode of
    79 -> KeyRight
    80 -> KeyLeft
    81 -> KeyDown
    82 -> KeyUp
    _ -> KeyOther

