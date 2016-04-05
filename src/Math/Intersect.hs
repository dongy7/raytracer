module Math.Intersect where

import Prelude hiding ((<*>), (<+>), (<->))

import Geometry.Object
import Geometry.Vector

type Solution = (Maybe Scalar, Maybe Scalar)
type RayScalar = Maybe Scalar

-- ((ray, solutions))
type Intersection = (Ray, RayScalar, Surface)

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
getMinSoln (Just a, Just b) = if a < b
                                 then Just a
                                 else Just b

-- Returns the intersection of ray with given surface
rayIntersect :: Ray -> Surface -> Intersection
rayIntersect (Ray e d ) (Sphere radius center material) = (Ray e d, minSoln, Sphere radius center material)
  where a = d <*> d
        b = 2 * (d <*> (e <-> center))
        c = (e <-> center) <*> (e <-> center) - radius*radius
        solns = solveQuad a b c
        minSoln = getMinSoln solns
rayIntersect (Ray e d) (Plane n p material) = (Ray e d, solns, Plane n p material)
  where numerator = (p <-> e) <*> n
        denominator = d <*> n
        quotient = numerator/denominator
        solns | (d <*> n) == 0 = Nothing
              | otherwise = getPosScalar quotient

-- Preserve positive values
getPosScalar :: Scalar -> Maybe Scalar
getPosScalar s = if s > 0
                    then Just s
                    else Nothing
