module Math.Shader where

import Prelude hiding ((<*>), (<+>), (<->))

import Math.SceneParams
import Geometry.Object
import Geometry.Vector

-- compute the overall shading for a point on surface
computeShading :: Surface -> Vector -> Colour
computeShading surf vec = addColour phong $ addColour lambertian ambiant
  where lambertian = diffuseShade surf vec
        ambiant = ambientShade surf
        phong = phongShade surf vec

-- compute the phong shading for a given point on surface
phongShade :: Surface -> Vector -> Colour
phongShade (Sphere rad center (Material amb dif surf pow)) vec = scaleColour surf k
  where vVec = e <-> vec
        lVec = lightSource <-> vec
        normV = normalise vVec
        normL = normalise lVec
        h = computeBisector normV normL
        normN = normalise $ computeSurfNorm (Sphere rad center (Material amb dif surf pow)) vec
        dot = normN <*> h
        k = intensity * ((max' (0, dot)) ** pow)
phongShade (Plane norm point (Material amb dif surf pow)) vec = scaleColour surf k
  where vVec = e <-> vec
        lVec = lightSource <-> vec
        normV = normalise vVec
        normL = normalise lVec
        h = computeBisector normV normL
        normN = normalise $ computeSurfNorm (Plane norm point (Material amb dif surf pow)) vec
        dot = normN <*> h
        k = intensity * ((max' (0, dot)) ** pow)

-- compute the diffuse shading for a given point on surface
diffuseShade :: Surface -> Vector -> Colour
diffuseShade (Sphere rad cen (Material am dif sur pow)) vec = scaleColour dif k
  where normN = normalise $ computeSurfNorm (Sphere rad cen (Material am dif sur pow)) vec
        normL = normalise lVec
        lVec = lightSource <-> vec
        dot = normL <*> normN
        k = intensity * (max' (0, dot))
diffuseShade (Plane norm point (Material am dif sur pow)) vec = scaleColour dif k
  where normN = normalise $ computeSurfNorm (Plane norm point (Material am dif sur pow)) vec
        normL = normalise l
        l = lightSource <-> vec
        dot = normL <*> normN
        k = intensity * (max' (0, dot))

-- compute the ambient shading for a given point on surface
ambientShade :: Surface -> Colour
ambientShade  (Sphere _ _ (Material amb _ _ _)) = scaleColour amb intensity
ambientShade  (Plane _ _ (Material amb _ _ _)) = scaleColour amb intensity

-- compute the normal for a point on a surface
computeSurfNorm :: Surface -> Vector -> Vector
computeSurfNorm (Sphere _ center _) vec = vec <-> center
computeSurfNorm (Plane normal _ _) _ = normal

computeSurfPoint :: Ray -> Scalar -> Vector
computeSurfPoint (Ray origin direction) scalar = origin <+> (mult direction scalar)

max' :: (Ord a) => (a, a) -> a
max' (x, y) = if x > y
                 then x
                 else y
