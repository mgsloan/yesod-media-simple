{-# LANGUAGE ViewPatterns #-}
import Codec.Picture
import Data.Time
import Yesod
import Yesod.Media
import Control.Monad.IO.Class (liftIO)

main = (serve :: LiteHandler (Image PixelRGB8) -> IO ()) $ do
    now <- liftIO getCurrentTime
    return $ generateImage (pixel now) 640 480

-- See the glsl at http://www.bidouille.org/prog/plasma
pixel :: UTCTime -> Int -> Int -> PixelRGB8
pixel (realToFrac . utctDayTime -> t) ((/ 100) . fromIntegral -> x) ((/ 100) . fromIntegral -> y) =
    PixelRGB8 (round $ 255 * r) (round $ 255 * g) (round $ 255 * b)
  where
    r = 1
    g = sin (pi * v)
    b = cos (pi * v)
    v = sin (x + t)
      + sin ((y + t) / 2)
      + sin ((x + y + t) / 2)
      + sin (sqrt (x' * x' + y' * y' + 1) + t)
    x' = x + sin (t / 2)
    y' = y + cos (t / 2)
