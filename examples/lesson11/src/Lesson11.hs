{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           SDL                    (($=))


windowSize :: (Int, Int)
windowSize = (640, 480)


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Lesson 11" windowSize $ \w ->
    C.withRenderer w $ \r -> do

      t <- C.loadTextureWithInfo r "./assets/dots.png"
      let doRender = draw r t

      whileM $ do
        ev <- SDL.pollEvents
        if C.hasQuitEvent ev
          then pure False
          else doRender >> pure True

      SDL.destroyTexture (fst t)


draw :: (MonadIO m) => SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> m ()
draw r (t, ti) = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r

  renderTexture r t (d `moveTo` mTL) (d `moveTo` pTL)
  renderTexture r t (d `moveTo` mTR) (d `moveTo` pTR)
  renderTexture r t (d `moveTo` mBL) (d `moveTo` pBL)
  renderTexture r t (d `moveTo` mBR) (d `moveTo` pBR)

  SDL.present r

  where
    (sw, sh) = windowSize

    tw :: Double
    th :: Double
    tw = fromIntegral $ SDL.textureWidth ti
    th = fromIntegral $ SDL.textureHeight ti

    d = C.mkRect 0 0 (round $ tw / 2) (round $ th / 2)

    mTL = (  0 ,   0)
    mTR = (100 ,   0)
    mBL = (  0 , 100)
    mBR = (100 , 100)

    px = sw - round (tw / 2)
    py = sh - round (th / 2)

    pTL = ( 0 ,  0)
    pTR = (px ,  0)
    pBL = ( 0 , py)
    pBR = (px , py)


moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (C.mkPoint x y) d


renderTexture
  :: (Integral a, MonadIO m)
  => SDL.Renderer
  -> SDL.Texture
  -> SDL.Rectangle a
  -> SDL.Rectangle a
  -> m ()

renderTexture r t mask pos =
  SDL.copy r t (Just $ fromIntegral <$> mask) (Just $ fromIntegral <$> pos)
