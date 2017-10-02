module Lesson12.Transitions
  ( stepWorld
  , increase
  , decrease
  , toggle
  , exit
  ) where

import Lesson12.Types


stepWorld :: World -> World
stepWorld w = w { colors = newColors }
  where
    newColors = (colors w) { redV = newRedV, greenV = newGreenV, blueV = newBlueV }
    newRedV   = updateColorValue (redT (toggles w)) (redV (colors w))
    newGreenV = updateColorValue (greenT (toggles w)) (greenV (colors w))
    newBlueV  = updateColorValue (blueT (toggles w)) (blueV (colors w))


updateColorValue :: Bool -> Double -> Double
updateColorValue p v = if p then v + 4 else v


increase :: Color -> World -> World
increase Red   = modifyColor redLens (+ 16)
increase Green = modifyColor greenLens (+ 16)
increase Blue  = modifyColor blueLens (+ 16)


decrease :: Color -> World -> World
decrease Red   = modifyColor redLens (flip (-) 16)
decrease Green = modifyColor greenLens (flip (-) 16)
decrease Blue  = modifyColor blueLens (flip (-) 16)


modifyColor :: Lens ColorValues Double -> (Double -> Double) -> World -> World
modifyColor lens func w = w { colors = colors' }
  where colors' = modL lens (colors w) func


modL :: Lens a b -> a -> (b -> b) -> a
modL lens record func = setL lens newValue record
  where value = getL lens record
        newValue = func value


redLens :: Lens ColorValues Double
redLens = Lens redV setRed
  where setRed x w = w { redV = x }


greenLens :: Lens ColorValues Double
greenLens = Lens greenV setGreen
  where setGreen x w = w { greenV = x }


blueLens :: Lens ColorValues Double
blueLens = Lens blueV setBlue
  where setBlue x w = w { blueV = x }


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
