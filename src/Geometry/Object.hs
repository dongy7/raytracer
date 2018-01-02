module Geometry.Object where
import Geometry.Vector
import Graphics.UI.GLUT hiding (Radius, Color)

type GLColor = Color3 GLfloat
type Color = (Scalar, Scalar, Scalar)
type ByteColor = (Int, Int, Int)

-- ambient diffuse surface power
data Material = Material
  {
    ambiant :: Color,
    diffuse :: Color,
    specular :: Color,
    specularPower :: Scalar,
    alpha :: Scalar
  }

data Surface =
  Sphere
  {
    radius :: Scalar,
    center :: Vector,
    material :: Material
  } |
  Plane
  {
    normal :: Vector,
    point :: Vector,
    material :: Material
  }

type Scene = [Surface]

-- Ray origin direction
data Ray = Ray Vector Vector

scaleColor :: Color -> Scalar -> Color
scaleColor (x, y, z) k = (x*k, y*k, z*k)

addColor :: Color -> Color -> Color
addColor (a, b, c) (x, y, z) = (a+x, b+y, c+z)

convertToByteColor :: Color -> ByteColor
convertToByteColor (a, b, c) = (convertToInt a, convertToInt b, convertToInt c)

convertToInt :: Scalar -> Int
convertToInt s =  fromIntegral (round s :: Int) :: Int

convertToFloat :: Scalar -> GLfloat
convertToFloat s = realToFrac s :: GLfloat

convertToFloatColor :: Color -> Color3 GLfloat
convertToFloatColor (x, y, z) = Color3 (convertToFloat x) (convertToFloat y) (convertToFloat z)
