module Math.Intersect (
  rayIntersect,
  toPosIntersection,
  Solution,
  RayScalar,
  Intersection,
  PosIntersection
) where

import Control.Applicative hiding ((<*>))
import Prelude hiding ((<*>))
import Geometry.Object
import Geometry.Vector

type Solution = (Maybe Scalar, Maybe Scalar)
type RayScalar = Maybe Scalar

-- ((ray, solutions))
type Intersection = (Ray, RayScalar, Surface)

-- positive intersection
type PosIntersection = (Ray, Scalar, Surface)

toPosIntersection :: Intersection -> Maybe PosIntersection
toPosIntersection (_, Nothing, _) = Nothing
toPosIntersection (r, Just x, s)
  | x > 0     = Just (r, x, s)
  | otherwise = Nothing

-- Solves ax^2 + bx + c = 0
-- only returns positive solutions
solveQuad :: Scalar -> Scalar -> Scalar -> Solution
solveQuad a b c = solns
  where solns | discriminant < 0 = (Nothing, Nothing)
              | otherwise = (fstRt, sndRt)
        discriminant = (b*b) - 4*a*c
        sqrtDiscriminant = sqrt discriminant
        rt1 = (-b + sqrtDiscriminant)/ (2*a)
        rt2 = (-b - sqrtDiscriminant)/ (2*a)
        fstRt = getPosScalar rt1
        sndRt = getPosScalar rt2

-- Returns the smallest positive root
getMinSoln :: Solution -> RayScalar
getMinSoln (Nothing, x) = x
getMinSoln (x, Nothing) = x
getMinSoln (a, b) = liftA2 min a b

-- Returns the intersection of ray with given surface
rayIntersect :: Ray -> Surface -> Intersection
rayIntersect ray@(Ray e d ) sphere@(Sphere radius center material) = (ray, getMinSoln solns, sphere)
  where a = d <*> d
        b = 2 * (d <*> (e <-> center))
        c = (e <-> center) <*> (e <-> center) - radius*radius
        solns = solveQuad a b c
rayIntersect ray@(Ray e d) plane@(Plane n p material) = (ray, solns, plane)
  where numerator = (p <-> e) <*> n
        denominator = d <*> n
        quotient = numerator/denominator
        solns | (d <*> n) == 0 = Nothing
              | otherwise = getPosScalar quotient

-- Preserve positive values
getPosScalar :: Scalar -> Maybe Scalar
getPosScalar c
  | c > 0     = Just c
  | otherwise = Nothing