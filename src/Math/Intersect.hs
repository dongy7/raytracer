module Math.Intersect where

import Prelude hiding ((<*>), (<+>), (<->))

import Geometry.Object
import Geometry.Vector

type Solution = (Maybe Scalar, Maybe Scalar)

-- ((ray origin, ray direction, solutions), surface color))
type Intersection = ((Vector, Vector, [(Scalar, Surface)]), Colour)


-- Solves ax^2 + bx + c = 0
solveQuad :: Scalar -> Scalar -> Scalar -> Solution
solveQuad a b c = solns
  where solns | discriminant < 0 = (Nothing, Nothing)
              | otherwise = (fstRt, sndRt)
        discriminant = (b*b) - 4*a*c
        sqrtDiscriminant = sqrt discriminant
        rt1 = (-b + sqrtDiscriminant)/ (2*a)
        rt2 = (-b - sqrtDiscriminant)/ (2*a)
        fstRt = if rt1 > 0
                   then Just rt1
                   else Nothing
        sndRt = if rt2 > 0
                   then Just rt2
                   else Nothing

solveQuadratic :: Scalar -> Scalar -> Scalar -> [Scalar]
solveQuadratic a b c = solns
  where solns  | discriminant < 0 = []
               | otherwise = filter (\x -> x > 0) [(-b + sqrtDiscriminant)/ (2*a), (-b - sqrtDiscriminant)/ (2*a)]
        discriminant = (b*b) - (4*a*c)
        sqrtDiscriminant = sqrt discriminant

rayIntersect :: Ray -> Surface -> Intersection
rayIntersect (Ray e d) (Sphere radius center (Material amb dif s p)) =
  ((e, d, combineSoln (minimum' $ solveQuadratic a b c) (Sphere radius center (Material amb dif s p))) , dif)
  where a = d <*> d
        b = 2 * (d <*> (e <-> center))
        c = (e <-> center) <*> (e <-> center) - radius*radius
rayIntersect (Ray e d) (Plane n p (Material amb dif s pow)) = ((e, d, solns), dif )
  where solns  | (d <*> n) == 0 = []
               | otherwise = filter (\(x, _) -> x > 0) [( numerator/denominator, Plane n p (Material amb dif s pow))]
        numerator = (p <-> e) <*> n
        denominator = d <*> n

minimum' :: (Ord a) => [a] -> [a]
minimum' [] = []
minimum' list = [ minimum list ]

combineSoln :: [a] -> b -> [(a, b)]
combineSoln [] _ = []
combineSoln (x:xs) b = [(x, b)] ++ combineSoln xs b
