module Math.Shader (
  computeShading,
  computeSurfNorm,
  computeSurfPoint,
  ambientShade,
  getAmbiant,
  getDiffuse,
  getSpecular,
  getSpecPower,
  getAlpha
) where

import Prelude hiding ((<*>))
import Math.SceneParams
import Geometry.Object
import Geometry.Vector

-- compute the overall shading for a point on surface
computeShading :: Surface -> Vector -> Color
computeShading surf vec = addColor phong $ addColor lambertian ambiant
  where lambertian = diffuseShade surf vec
        ambiant = ambientShade surf
        phong = phongShade surf vec

-- compute the phong shading for a given point on surface
phongShade :: Surface -> Vector -> Color
phongShade surf vec = scaleColor spec k
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
diffuseShade :: Surface -> Vector -> Color
diffuseShade surf vec = scaleColor dif k
  where normN = normalise $ computeSurfNorm surf vec
        normL = normalise lVec
        lVec = lightSource <-> vec
        dot = normL <*> normN
        dif = getDiffuse surf
        k = intensity * max' (0, dot)

-- compute the ambient shading for a given point on surface
ambientShade :: Surface -> Color
ambientShade  (Sphere _ _ mat) = scaleColor (ambiant mat) intensity
ambientShade  (Plane _ _ mat) = scaleColor (ambiant mat) intensity

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
getSpecPower (Sphere _ _ material) = specularPower material
getSpecPower (Plane _ _ material) = specularPower material

getAmbiant :: Surface -> Vector
getAmbiant (Sphere _ _ material) = ambiant material
getAmbiant (Plane _ _ material) = ambiant material

getDiffuse :: Surface -> Vector
getDiffuse (Sphere _ _ material) = diffuse material
getDiffuse (Plane _ _ material) = diffuse material

getSpecular :: Surface -> Vector
getSpecular (Sphere _ _ material) = specular material
getSpecular (Plane _ _ material) = specular material

getAlpha :: Surface -> Scalar
getAlpha (Sphere _ _ material) = alpha material
getAlpha (Plane _ _ material) = alpha material

