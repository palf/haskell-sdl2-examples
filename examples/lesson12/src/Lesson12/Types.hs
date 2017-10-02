module Lesson12.Types
  ( World (..)
  , ColorValues (..)
  , ColorToggles (..)
  , Color (..)
  , Intent (..)
  , Lens (..)
  ) where


data World = World
  { colors  :: ColorValues
  , toggles :: ColorToggles
  , exiting :: Bool
  } deriving (Show)


data ColorValues = ColorValues
  { redV   :: Double
  , greenV :: Double
  , blueV  :: Double
  } deriving (Show)


data ColorToggles = ColorToggles
  { redT   :: Bool
  , greenT :: Bool
  , blueT  :: Bool
  } deriving (Show)


data Color = Red | Green | Blue
  deriving (Show)


data Intent
  = Increase Color
  | Decrease Color
  | Toggle Color
  | Idle
  | Quit
  deriving (Show)


data Lens a b = Lens
  { getL :: a -> b
  , setL :: b -> a -> a
  }
