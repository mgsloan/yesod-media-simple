{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import qualified Codec.Picture as Juicy
import Control.Newtype (pack, unpack)
import Control.Lens ((^.), unwrapping)
import Data.Default
import Data.List
import Data.Monoid.Inf (Inf(..), PosInf)
import Data.Ord
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.ThreeD.Types
import qualified Diagrams.ThreeD as ThreeD
import Diagrams.Backend.Cairo
import Yesod.Media

--TODO:
-- * Add a primitive to diagrams that encompasses the entire plane.

main :: IO ()
main = serve $ debugTrace ray scene
  where
    ray = case 1 of
        0 -> Ray (p3 (3, 3, 3)) (r3 (-1, -1, -1))
        1 -> Ray (p3 (0, 4, 4)) (r3 (0, -1, -1))
        _ -> Ray (p3 (0, 0, 5)) (r3 (0, 0, -1))
    scene = cube

-- The algorithm will look something like:
--
-- 1. Find the first intersection of the ray with the 3 ortho shapes.
--
-- 2. Prefer the furthest intersection, since we're intersecting
--    these shapes (which means that further out bits get removed).
--
-- 3. If the point isn't included in the other shapes, then it might
--    be inside a hole - continue tracing those shapes from this point.

data TraceResults = TraceResults
    { traceSegs :: [(ArrowHT, Ray R3)] -- ^ Drawn in the debugging results.
    , traceOutput :: Juicy.Image Juicy.PixelRGB8
    }

--TODO: use the ray's vector as a camera angle

runTrace :: Int -> Int -> Ray R3 -> Shape b -> TraceResults
runTrace w h (normalizedRay -> cameraRay) Shape{..} =
    TraceResults
    { traceSegs = rays
    , traceOutput = img
    }
  where
    (rays, img) = Juicy.generateFoldImage go [] w h
    go debugRays cx cy =
        ( case s of
            Finite x | cx `mod` 5 == 0 && cy `mod` 5 == 0 ->
                (noHead, Ray (rayOrigin ray) (x *^ rayVector ray)) : debugRays
            Infinity -> (dart, Ray (rayOrigin ray) (5 *^ rayVector ray)) : debugRays
            _ -> debugRays
        , case which of
            0 -> Juicy.PixelRGB8 val 0 0
            1 -> Juicy.PixelRGB8 0 val 0
            _ -> Juicy.PixelRGB8 0 0 val
        )
      where
        val = case s of
            Finite x -> round (x * 40)
            Infinity -> 0
        --TODO: turn null vector ray tests into inclusion tests
        (which, s) = maximumBy (comparing snd) $
            zipWith (\i (f, x) -> (i, traceRay (transformRay f ray) x)) [0..]
            [ (xvec, side)
            , (yvec, top)
            , (zvec, front)
            ]
        --TODO: Just use rays a plane instead of rotations.
        ray = Ray
            { rayOrigin = rayOrigin cameraRay
            , rayVector =
                apply (ThreeD.rotationAbout origin updir yaw) $
                apply (ThreeD.rotationAbout origin rightdir pitch) (rayVector cameraRay)
            }
        coef = 1
        yaw = Turn (fromIntegral (cx - w `div` 2) / fromIntegral w * coef)
        pitch = Turn (fromIntegral (cy - h `div` 2) / fromIntegral h * coef)
        Spherical (Turn about) (Turn toward) = ThreeD.direction $ rayVector cameraRay
        updir = Spherical (Turn about) (Turn (toward + 1/4))
        rightdir = Spherical (Turn (about + 1/4)) (Turn (toward + 1/4))

debugTrace :: Ray R3 -> Shape Cairo -> IO (Diagram Cairo R2)
debugTrace ray shape@Shape{..} = do
    results <- imageToDiagram output
    return $ lw 0.025 $
        (ortho yvec top ||| viewport results)
        ===
        (ortho zvec front ||| ortho xvec side)
  where
    ortho f contents =
        viewport $
            mconcat (map (\(h, r) -> drawRay h (transformRay f r)) rays) <>
            contents # lw 0.1
    TraceResults rays output = runTrace 300 300 ray shape

viewport :: Diagram Cairo R2 -> Diagram Cairo R2
viewport contents =
    withEnvelope (stroke viewbox :: Diagram Cairo R2) $
        clipBy viewbox contents <>
        centerXY viewbox
  where
    viewbox = square 10

-- Shape

data Shape b = Shape
    { side  :: Diagram b R2
    , top   :: Diagram b R2
    , front :: Diagram b R2
    }

steinmetz :: Renderable (Path R2) b => Shape b
steinmetz = Shape (circle 2) (square 4) (circle 2)

cube :: Renderable (Path R2) b => Shape b
cube = Shape (square 1) (square 1) (square 1)

type instance V (Shape b) = R3

{-
instance Transformable (Shape b) where
    transform t x =
-}

-- Ray

data Ray v = Ray
    { rayOrigin :: (Point v)
    , rayVector :: v
    }

normalizedRay :: (InnerSpace v, s ~ Scalar v, Floating s) => Ray v -> Ray v
normalizedRay (Ray p v) = Ray p (normalized v)

transformRay :: (v -> v') -> Ray v -> Ray v'
transformRay f (Ray p v) = Ray (onPoint f p) (f v)

drawRay :: Renderable (Path R2) b => ArrowHT -> Ray R2 -> Diagram b R2
drawRay h (Ray p v) =
    arrowAt' (def { _arrowHead = h }) p v <>
    mconcat (map (dash 0.2) [0..len-1]) <>
    mconcat (map (dash 0.1) [0,0.25..len-0.25])
  where
    len = magnitude v
    dir = direction v
    norm = v ^/ len
    dash w t = moveTo (p .+^ t *^ norm) $ rotateBy dir $ vrule w

traceRay :: (v ~ V a, Traced a) => Ray v -> a -> PosInf (Scalar v)
traceRay (Ray p v) x = ((getTrace x)^.unwrapping Trace) p v

-- Utilities

{- xtrans, ytrans, ztrans :: T3 -> T2
xtrans t = fromLinear s (linv s)
  where
    s = <->
    ((_, xvec -> y, xvec -> z), tr) = onBasis t
-}

xvec, yvec, zvec :: R3 -> R2
xvec (unr3 -> (_, py, pz)) = r2 (pz, py)
yvec (unr3 -> (px, _, pz)) = r2 (px, pz)
zvec (unr3 -> (px, py, _)) = r2 (px, py)

--FIXME: "over wrapped %~" from Control.Lens should be sufficient
-- (need a Wrapped instance for Point)
onPoint :: (v -> v') -> Point v -> Point v'
onPoint f = pack . f . unpack
