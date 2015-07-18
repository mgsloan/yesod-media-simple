{-# LANGUAGE ViewPatterns #-}
import Codec.Picture
import Data.Time
import Data.Word
import Yesod
import Yesod.Media.Simple

main = serve $ do
    now <- getCurrentTime
    return $ PixelList 640 480
      [ [ pixel now x y
        | x <- [1..640]
        ]
      | y <- [1..480]
      ]

-- See the glsl at http://www.bidouille.org/prog/plasma
pixel :: UTCTime -> Int -> Int -> (Word8, Word8, Word8)
pixel (realToFrac . utctDayTime -> t) ((/ 100) . fromIntegral -> x) ((/ 100) . fromIntegral -> y) =
    (round $ 255 * r, round $ 255 * g, round $ 255 * b)
  where
    r = 1
    g = sin (pi * v) :: Float
    b = cos (pi * v)
    v = sin (x + t)
      + sin ((y + t) / 2)
      + sin ((x + y + t) / 2)
      + sin (sqrt (x' * x' + y' * y' + 1) + t)
    x' = x + sin (t / 2)
    y' = y + cos (t / 2)
