module Render.Raytracer where

import Geometry.Object
import Geometry.Vector
import Math.SceneParams
import Math.Intersect
import Math.Shader

computePixelPairColor :: Scene -> (Scalar, Scalar) -> Color
computePixelPairColor scene (i, j) = computePixelColor scene i j

computePixelColor :: Scene -> Scalar -> Scalar -> Color
computePixelColor scene i j = gammaCorrect color gamma
  where color = colorPoint depth intersection scene
        intersection = rayIntersectScene (computeRay i j) scene

-- generates the viewing ray for a given pixel coord
computeRay :: Scalar -> Scalar -> Ray
computeRay i j = Ray e (s <-> e)
  where s = e <+> (mult u uCoord) <+> (mult v vCoord) <-> (mult w d)
        uCoord = l + (r-l) * (i+0.5)/(fromIntegral width)
        vCoord = b + (t-b) * (j+0.5)/(fromIntegral height)

getReflectedColor :: Int -> Maybe PosIntersection -> Scene -> Color
getReflectedColor = colorPoint

-- gets intersection color with reflection
colorPoint :: Int -> Maybe PosIntersection -> Scene -> Color
colorPoint (-1) _ _     = (0.0, 0.0, 0.0)
colorPoint _ Nothing _  = (0.0, 0.0, 0.0)
colorPoint depth (Just (Ray e d, s, surf)) scene = addColor reflectColor phongColor
  where surfPoint = computeSurfPoint (Ray e d) s
        surfNorm = normalise $ computeSurfNorm surf surfPoint
        fixedSurfPoint = surfPoint <+> (mult surfNorm epsilon)
        reflectDir = computeReflection d surfNorm
        reflectRay = Ray fixedSurfPoint reflectDir
        reflectIntersection = rayIntersectScene reflectRay scene
        alpha = getAlpha surf
        reflectColor = scaleColor (getReflectedColor (depth-1) reflectIntersection scene) alpha
        phongColor = getShadeColor (Ray e d, s, surf) fixedSurfPoint alpha scene


getShadeColor :: PosIntersection -> Vector -> Scalar -> Scene -> Color
getShadeColor (ray, s, surf) surfPoint alpha scene
  | isBlocked surf scene ray s = ambientShade surf
  | otherwise                  = scaleColor (computeShading surf (computeSurfPoint ray s)) (1-alpha)

-- returns intersection color for a possible intersection
getIntersectColor :: Maybe PosIntersection -> Scene -> Color
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
rayBlocked ray scene = case minIntersect of
                         Nothing -> False
                         _       -> True
  where minIntersect = rayIntersectScene ray scene

-- returns the closest intersection
rayIntersectScene :: Ray -> Scene -> Maybe PosIntersection
rayIntersectScene ray scene = getMinIntersect [rayIntersect ray surf | surf <- scene]

-- returns the minimum intersection
getMinIntersect :: [Intersection] -> Maybe PosIntersection
getMinIntersect [] = Nothing
getMinIntersect xs = toPosIntersection $ foldl1 min' xs

-- returns min of two maybe values
min' :: Intersection -> Intersection -> Intersection
min' (_, Nothing, _) b@(_, Nothing, _) = b
min' (_, Nothing, _) b@(_, _, _)       = b
min' a@(_, _, _) (_, Nothing, _)       = a
min' a@(_, Just x, _) b@(_, Just y, _)
  | x < y     = a
  | otherwise = b

-- computes gamma corrected color
gammaCorrect :: Color -> Scalar -> Color
gammaCorrect (x, y, z) g = (x', y', z')
  where x' = x ** (1/g)
        y' = y ** (1/g)
        z' = z ** (1/g)
