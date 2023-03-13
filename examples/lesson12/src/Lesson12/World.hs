module Lesson12.World
  ( World

  , exiting
  , colors
  , ColorValues (..)

  , initialWorld
  , updateWorld
  ) where

import           Data.Foldable  (foldl')
import           Lesson12.Intents


data Lens a b = Lens
  { getL :: a -> b
  , setL :: b -> a -> a
  }


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


data World = World
  { colors  :: ColorValues
  , toggles :: ColorToggles
  , exiting :: Bool
  } deriving (Show)


initialWorld :: World
initialWorld = World
  { colors = ColorValues
    { redV   = 128
    , greenV = 128
    , blueV  = 128
    }
  , toggles = ColorToggles
    { redT   = False
    , greenT = False
    , blueT  = False
    }
  , exiting = False
  }


redLens :: Lens ColorValues Double
redLens = Lens redV setRed
  where setRed x w = w { redV = x }


greenLens :: Lens ColorValues Double
greenLens = Lens greenV setGreen
  where setGreen x w = w { greenV = x }


blueLens :: Lens ColorValues Double
blueLens = Lens blueV setBlue
  where setBlue x w = w { blueV = x }


updateColorValue :: Bool -> Double -> Double
updateColorValue p v = if p then v + 4 else v


stepWorld :: World -> World
stepWorld w = w { colors = newColors }
  where
    newColors = (colors w) { redV = newRedV, greenV = newGreenV, blueV = newBlueV }
    newRedV   = updateColorValue (redT (toggles w)) (redV (colors w))
    newGreenV = updateColorValue (greenT (toggles w)) (greenV (colors w))
    newBlueV  = updateColorValue (blueT (toggles w)) (blueV (colors w))


modL :: Lens a b -> a -> (b -> b) -> a
modL lens record func = setL lens newValue record
  where value = getL lens record
        newValue = func value


modifyColor :: Lens ColorValues Double -> (Double -> Double) -> World -> World
modifyColor lens func w = w { colors = colors' }
  where colors' = modL lens (colors w) func


increase :: Color -> World -> World
increase Red   = modifyColor redLens (+ 16)
increase Green = modifyColor greenLens (+ 16)
increase Blue  = modifyColor blueLens (+ 16)


decrease :: Color -> World -> World
decrease Red   = modifyColor redLens (flip (-) 16)
decrease Green = modifyColor greenLens (flip (-) 16)
decrease Blue  = modifyColor blueLens (flip (-) 16)


toggle :: Color -> World -> World
toggle Red w = w { toggles = newToggles }
  where newRedT = not $ redT (toggles w)
        newToggles = (toggles w) { redT = newRedT }


toggle Green w = w { toggles = newToggles }
  where newGreenT = not $ greenT (toggles w)
        newToggles = (toggles w) { greenT = newGreenT }


toggle Blue w = w { toggles = newToggles }
  where newBlueT = not $ blueT (toggles w)
        newToggles = (toggles w) { blueT = newBlueT }


exit :: World -> World
exit w = w { exiting = True }


runIntent :: Intent -> World -> World
runIntent (Increase color) = increase color
runIntent (Decrease color) = decrease color
runIntent (Toggle color)   = toggle color
runIntent Idle             = id
runIntent Quit             = exit


updateWorld :: World -> [Intent] -> World
updateWorld w
  = stepWorld
  . foldl' (flip runIntent) w
