module Math.Shader where

import Prelude hiding ((<*>))
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
phongShade surf vec = scaleColour spec k
  where vVec = e <-> vec
        lVec = lightSource <-> vec
        normV = normalise vVec
        normL = normalise lVec
        h = computeBisector normV normL
        normN = normalise $ computeSurfNorm surf vec
        dot = normN <*> h
        pow = getSpecPower surf
        spec = getSpecular surf
        k = intensity * (max' (0, dot) ** pow)

-- compute the diffuse shading for a given point on surface
diffuseShade :: Surface -> Vector -> Colour
diffuseShade surf vec = scaleColour dif k
  where normN = normalise $ computeSurfNorm surf vec
        normL = normalise lVec
        lVec = lightSource <-> vec
        dot = normL <*> normN
        dif = getDiffuse surf
        k = intensity * max' (0, dot)

-- compute the ambient shading for a given point on surface
ambientShade :: Surface -> Colour
ambientShade  (Sphere _ _ (Material amb _ _ _ _)) = scaleColour amb intensity
ambientShade  (Plane _ _ (Material amb _ _ _ _)) = scaleColour amb intensity

-- compute the normal for a point on a surface
computeSurfNorm :: Surface -> Vector -> Vector
computeSurfNorm (Sphere _ center _) vec = vec <-> center
computeSurfNorm (Plane normal _ _) _ = normal

computeSurfPoint :: Ray -> Scalar -> Vector
computeSurfPoint (Ray origin direction) scalar = origin <+> mult direction scalar

max' :: (Ord a) => (a, a) -> a
max' (x, y) = if x > y
                 then x
                 else y

getSpecPower :: Surface -> Scalar
getSpecPower (Sphere _ _ (Material _ _ _ x _)) = x
getSpecPower (Plane _ _ (Material _ _ _ x _)) = x

getAmbiant :: Surface -> Vector
getAmbiant (Sphere _ _ (Material x _ _ _ _)) = x
getAmbiant (Plane _ _ (Material x _ _ _ _)) = x

getDiffuse :: Surface -> Vector
getDiffuse (Sphere _ _ (Material _ x _ _ _)) = x
getDiffuse (Plane _ _ (Material _ x _ _ _)) = x

getSpecular :: Surface -> Vector
getSpecular (Sphere _ _ (Material _ _ x _ _)) = x
getSpecular (Plane _ _ (Material _ _ x _ _)) = x

getAlpha :: Surface -> Scalar
getAlpha (Sphere _ _ (Material _ _ _ _ x)) = x
getAlpha (Plane _ _ (Material _ _ _ _ x)) = x

