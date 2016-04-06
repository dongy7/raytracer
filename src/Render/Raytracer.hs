module Render.Raytracer where

import Geometry.Object
import Geometry.Vector
import Math.SceneParams
import Math.Intersect

-- generates the viewing ray for a given pixel coord
computeRay :: Scalar -> Scalar -> Ray
computeRay i j = ray
  where ray = Ray e (s <-> e)
        s = e <+> (mult u uCoord) <+> (mult v vCoord) <-> (mult w d)
        uCoord = l + (r-l) * (i+0.5)/(fromIntegral width)
        vCoord = b + (t-b) * (j+0.5)/(fromIntegral height)


-- returns the closest intersection
rayIntersectScene :: Ray -> Scene -> Maybe Intersection
rayIntersectScene ray scene = getMinIntersect intersections
  where intersections = [rayIntersect ray surf | surf <- scene]

-- returns the minimum intersection
getMinIntersect :: [Intersection] -> Maybe Intersection
getMinIntersect [] = Nothing
getMinIntersect xs = if isPos min
                        then Just min
                        else Nothing
  where min = foldl1 minIntersection xs

-- returns min of two maybe values
minIntersection :: Intersection -> Intersection -> Intersection
minIntersection (r1, Just a, s1) (r2, Just b, s2) = if a < b
                                                             then (r1, Just a, s1)
                                                             else (r2, Just b, s2)
minIntersection (r1, Just a, s1) (_, Nothing, s2) = (r1, Just a, s1)
minIntersection (_, Nothing, _) (r2, Just b, s2) = (r2, Just b, s2)


isPos :: Intersection -> Bool
isPos (_, Nothing, _) = False
isPos (_, Just a, _) = a > 0


-- computes gamma corrected color
gammaCorrect :: Colour -> Scalar -> Colour
gammaCorrect (x, y, z) gamma = (x', y', z')
  where x' = x ** (1/gamma)
        y' = y ** (1/gamma)
        z' = z ** (1/gamma)