module Math.Shader where

import Prelude hiding ((<*>), (<+>), (<->))

import Math.SceneParams
import Geometry.Object
import Geometry.Vector

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

computeSurfNorm :: Surface -> Vector -> Vector
computeSurfNorm (Sphere _ center _) vec = vec <-> center
computeSurfNorm (Plane normal _ _) _ = normal

max' :: (Ord a) => (a, a) -> a
max' (x, y) = if x > y
                 then x
                 else y
