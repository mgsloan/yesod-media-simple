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
import Data.Monoid.Inf (Inf(..), PosInf)
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.ThreeD.Types
import Diagrams.Backend.Cairo
import Yesod.Media

--TODO:
-- * Add a primitive to diagrams that encompasses the entire plane.

main :: IO ()
main = serve $ debugTrace ray scene # lw 0.025
  where
    ray = Ray (p3 (0.3, 3, 3)) (r3 (0, -1.25, -1))
    scene = steinmetz

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

runTrace :: Ray R3 -> Shape b -> TraceResults
runTrace cameraRay Shape{..} =
    TraceResults
    { traceSegs = rays
    , traceOutput = img
    }
  where
    (rays, img) = Juicy.generateFoldImage go [] 10 10
    go debugRays ox oy =
        ( case s of
            Finite x -> (noHead, Ray (rayOrigin ray) (x *^ rayVector ray)) : debugRays
            Infinity -> (dart, Ray (rayOrigin ray) (5 *^ rayVector ray)) : debugRays
        , Juicy.PixelRGB8 val val val
        )
      where
        val = case s of
            Finite x -> round x
            Infinity -> 0
        --TODO: turn null vector ray tests into inclusion tests
        s = maximum $ map (\(f, x) -> traceRay (transformRay f ray) x)
            [ (xvec, side)
            , (yvec, top)
            , (zvec, front)
            ]
        --TODO: non orthographic camera.
        ray = Ray
            { rayOrigin = rayOrigin cameraRay .+^ r3 (fromIntegral ox / 4 - 1, fromIntegral oy / 4 - 4, 0)
            , rayVector = r3 (0.1, 0.1, -1)
            }

debugTrace :: Ray R3 -> Shape Cairo -> Diagram Cairo R2
debugTrace (normalizedRay -> ray) shape@Shape{..} =
    (ortho yvec top ||| viewport results)
    ===
    (ortho zvec front ||| ortho xvec side)
  where
    ortho f contents =
        viewport $
            mconcat (map (\(h, r) -> drawRay h (transformRay f r)) rays) <>
            contents # lw 0.1
    TraceResults rays output = runTrace ray shape
    results = mempty

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
