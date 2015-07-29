module Shared.Input.Keys (
    KeyPress (..),
    getKey
) where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types

data KeyPress
    = A
    | D
    | E
    | Q
    | S
    | W
    | KeyRight
    | KeyLeft
    | KeyDown
    | KeyUp
    | Unknown deriving (Eq, Show)

getKey :: SDL.Keysym -> KeyPress
getKey keysym = case keysymScancode keysym of
    4  -> A
    7  -> D
    8  -> E
    20 -> Q
    22 -> S
    26 -> W
    79 -> KeyRight
    80 -> KeyLeft
    81 -> KeyDown
    82 -> KeyUp
    _  -> Unknown

