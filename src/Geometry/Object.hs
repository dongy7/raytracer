module Geometry.Object where
import Geometry.Vector

type Colour = (Scalar, Scalar, Scalar)
type ByteColour = (Int, Int, Int)
type Radius = Scalar
type Center = Vector

-- ambient diffuse surface power
data Material = Material Colour Colour Colour Scalar

data Surface = Sphere Radius Center Material | Plane (Scalar, Scalar, Scalar) (Scalar, Scalar, Scalar) Material

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
