module Geometry.Object where
import Geometry.Vector
import Graphics.UI.GLUT hiding (Radius)

type GLColor = Color3 GLfloat
type Colour = (Scalar, Scalar, Scalar)
type ByteColour = (Int, Int, Int)
type Radius = Scalar
type Center = Vector
type Ambient = Colour
type Diffuse = Colour
type Specular = Colour
type SpecularPower = Scalar

-- ambient diffuse surface power
data Material = Material Ambient Diffuse Specular SpecularPower

-- data Sphere = Sphere Radius Center
-- data Plane = Plane (Scalar, Scalar, Scalar) (Scalar, Scalar, Scalar)
-- data Shape = Sphere | Plane

-- Plane normal point
data Surface = Sphere Radius Center Material | Plane (Scalar, Scalar, Scalar) (Scalar, Scalar, Scalar) Material

type Scene = [Surface]

-- Ray origin direction
data Ray = Ray Vector Vector

scaleColour :: Colour -> Scalar -> Colour
scaleColour (x, y, z) k = (x*k, y*k, z*k)

addColour :: Colour -> Colour -> Colour
addColour (a, b, c) (x, y, z) = (a+x, b+y, c+z)

convertToByteColour :: Colour -> ByteColour
convertToByteColour (a, b, c) = (convertToInt a, convertToInt b, convertToInt c)

convertToInt :: Scalar -> Int
convertToInt s =  fromIntegral (round s :: Int) :: Int

convertToFloat :: Scalar -> GLfloat
convertToFloat s = realToFrac s :: GLfloat

convertToFloatColor :: Colour -> Color3 GLfloat
convertToFloatColor (x, y, z) = Color3 (convertToFloat x) (convertToFloat y) (convertToFloat z)
