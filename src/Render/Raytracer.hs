module Render.Raytracer where

import Geometry.Object
import Geometry.Vector
import Math.SceneParams
import Math.Intersect
import Math.Shader


computePixelPairColor :: Scene -> (Scalar, Scalar) -> Colour
computePixelPairColor scene (i, j) = computePixelColor scene i j

computePixelColor :: Scene -> Scalar -> Scalar -> Colour
computePixelColor scene i j = gammaCorrect color gamma
  where color = colorPoint depth intersection scene
        intersection = rayIntersectScene (computeRay i j) scene

-- generates the viewing ray for a given pixel coord
computeRay :: Scalar -> Scalar -> Ray
computeRay i j = ray
  where ray = Ray e (s <-> e)
        s = e <+> (mult u uCoord) <+> (mult v vCoord) <-> (mult w d)
        uCoord = l + (r-l) * (i+0.5)/(fromIntegral width)
        vCoord = b + (t-b) * (j+0.5)/(fromIntegral height)

getReflectedColor :: Int -> Maybe PosIntersection -> Scene -> Colour
getReflectedColor = colorPoint

-- gets intersection color with reflection
colorPoint :: Int -> Maybe PosIntersection -> Scene -> Colour
colorPoint (-1) _ _ = (0.0, 0.0, 0.0)
colorPoint _ Nothing _  = (0.0, 0.0, 0.0)
colorPoint depth (Just (Ray e d, s, surf)) scene = addColour reflectColor phongColor
  where surfPoint = computeSurfPoint (Ray e d) s
        surfNorm = normalise $ computeSurfNorm surf surfPoint
        fixedSurfPoint = surfPoint <+> (mult surfNorm epsilon)
        reflectDir = computeReflection d surfNorm
        reflectRay = Ray fixedSurfPoint reflectDir
        reflectIntersection = rayIntersectScene reflectRay scene
        alpha = getAlpha surf
        reflectColor = scaleColour (getReflectedColor (depth-1) reflectIntersection scene) alpha
        phongColor = getShadeColor (Ray e d, s, surf) fixedSurfPoint alpha scene


getShadeColor :: PosIntersection -> Vector -> Scalar -> Scene -> Colour
getShadeColor (Ray e d, s, surf) surfPoint alpha scene = if (isBlocked surf scene (Ray e d) s)
  then ambientShade surf
  else scaleColour (computeShading surf (computeSurfPoint (Ray e d) s)) (1-alpha)

-- returns intersection color for a possible intersection
getIntersectColor :: Maybe PosIntersection -> Scene -> Colour
getIntersectColor m scene = case m of
  Nothing -> (0.0, 0.0, 0.0)
  Just (Ray origin direction, s, surf) -> if (isBlocked surf scene (Ray origin direction) s)
                                             then ambientShade surf
                                             else computeShading surf (computeSurfPoint (Ray origin direction) s)

-- returns true if ray is blocked by an object in the scene
isBlocked :: Surface -> Scene -> Ray -> Scalar -> Bool
isBlocked surf scene (Ray origin direction) scalar = rayBlocked (Ray fixedSurfPoint shadowDirection) scene
  where surfPoint = origin <+> (mult direction scalar)
        shadowDirection = lightSource <-> surfPoint
        normVec = computeSurfNorm surf surfPoint
        fixedSurfPoint = surfPoint <+> (mult normVec epsilon)


rayBlocked :: Ray -> Scene -> Bool
rayBlocked ray scene = isValid minIntersect
  where minIntersect = rayIntersectScene ray scene

isValid :: Maybe PosIntersection -> Bool
isValid Nothing = False
isValid (Just _) = True

-- returns the closest intersection
rayIntersectScene :: Ray -> Scene -> Maybe PosIntersection
rayIntersectScene ray scene = getMinIntersect intersections
  where intersections = [rayIntersect ray surf | surf <- scene]

-- returns the minimum intersection
getMinIntersect :: [Intersection] -> Maybe PosIntersection
getMinIntersect [] = Nothing
getMinIntersect xs = toPosIntersection minIntersect
  where minIntersect = foldl1 minIntersection xs

-- returns min of two maybe values
minIntersection :: Intersection -> Intersection -> Intersection
minIntersection (r1, Just x, s1) (r2, Just y, s2) = if x < y
                                                             then (r1, Just x, s1)
                                                             else (r2, Just y, s2)
minIntersection (r1, Just x, s1) (_, Nothing, _) = (r1, Just x, s1)
minIntersection (_, Nothing, _) (r2, Just y, s2) = (r2, Just y, s2)
minIntersection (_, Nothing, _) (r2, Nothing, s2) = (r2, Nothing, s2)


isPos :: Intersection -> Bool
isPos (_, Nothing, _) = False
isPos (_, Just x, _) = x > 0


-- computes gamma corrected color
gammaCorrect :: Colour -> Scalar -> Colour
gammaCorrect (x, y, z) g = (x', y', z')
  where x' = x ** (1/g)
        y' = y ** (1/g)
        z' = z ** (1/g)
