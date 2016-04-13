module Geometry.Vector (
  Scalar,
  Vector,
  (<+>),
  (<->),
  (<*>),
  mult,
  vecSum,
  normalise,
  computeBisector,
  computeReflection
) where

import Prelude hiding ((<*>))

type Scalar = Double
type Vector = (Scalar, Scalar, Scalar)

(<+>) :: Vector -> Vector -> Vector
(x1, y1, z1) <+> (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

(<->) :: Vector -> Vector -> Vector
(x1, y1, z1) <-> (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

(<*>) :: Vector -> Vector -> Scalar
(x1, y1, z1) <*> (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

mult :: Vector -> Scalar -> Vector
mult (x, y, z) c = (c*x, c*y, c*z)

vecSum :: Vector -> Scalar
vecSum (x, y, z) = x + y + z

mag :: Vector -> Scalar
mag (x, y, z) = sqrt $ x*x + y*y + z*z

normalise :: Vector -> Vector
normalise (x, y, z) = (x/m, y/m, z/m)
  where m = mag (x, y, z)

computeBisector :: Vector -> Vector -> Vector
computeBisector x y = bisector
  where sumVec = x <+> y
        sumMag = mag sumVec
        bisector = mult sumVec (1/sumMag)

-- ray-direction surface-normal
-- returns the reflection direction
computeReflection :: Vector -> Vector -> Vector
computeReflection direction normal = direction <-> mult normal (2* (direction <*> normal))
