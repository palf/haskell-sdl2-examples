module Lesson12.Intents where


data Color = Red | Green | Blue
  deriving (Show)


data Intent
  = Increase Color
  | Decrease Color
  | Toggle Color
  | Idle
  | Quit
  deriving (Show)
