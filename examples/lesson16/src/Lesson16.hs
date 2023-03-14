{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Font

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text
import           Foreign.C.Types
import           GHC.Word
import           SDL                    (($=))


colorBlack :: SDL.V4 Word8
colorBlack = SDL.V4 0 0 0 0


colorWhite :: SDL.V4 Word8
colorWhite = SDL.V4 maxBound maxBound maxBound maxBound


loadFontSurface :: (MonadIO m) => FilePath -> SDL.Font.PointSize -> Text -> m (SDL.Surface , SDL.V2 CInt)
loadFontSurface path size text = do
  font <- SDL.Font.load path size
  surf <- SDL.Font.shaded font colorBlack colorWhite text
  dim <- SDL.surfaceDimensions surf

  pure (surf, dim)


draw :: (MonadIO m) => SDL.Renderer -> (SDL.Texture, SDL.V2 CInt) -> m ()
draw r (t, SDL.V2 tw th) = do
  SDL.rendererDrawColor r $= colorWhite
  SDL.clear r

  SDL.copy r t Nothing (Just pos)
  SDL.present r

  where
    s = C.mkRect 0 0 (fromIntegral tw :: Double) (fromIntegral th)
    w = C.mkRect 0 0 640 480
    pos = round <$> C.centerWithin s w


main :: IO ()
main = C.withSDL $ do
  SDL.Font.initialize

  C.setHintQuality
  C.withWindow "Lesson 16" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      (surf, dim) <- loadFontSurface "./assets/lazy.ttf" 32 "some fish are delicious"
      t <- SDL.createTextureFromSurface r surf

      draw r (t, dim)
      whileM $ not . C.hasQuitEvent <$> SDL.pollEvents

      SDL.destroyTexture t

  SDL.Font.quit
