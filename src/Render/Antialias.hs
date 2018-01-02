module Render.Antialias where

import Geometry.Vector
import Geometry.Object
import Math.SceneParams
import Render.Raytracer
import System.Random

boxFilter :: Scene -> Scalar -> Scalar -> Color
boxFilter scene i j = getAverageColor $ map (computePixelPairColor scene) randomPixels
  where randomPixels = genRandomPixels i j

-- get average color from color list
getAverageColor :: [Color] -> Color
getAverageColor colors = scaleColor colorSum (1/len)
  where colorSum = foldl addColor (0, 0, 0) colors
        len = fromIntegral $ length colors

-- generates random sample of pixels within the boundaries of a given pixel
genRandomPixels :: Scalar -> Scalar -> [(Scalar, Scalar)]
genRandomPixels i j = map (\(x,y) -> (x+i, y+j)) $ zip randomXs randomYs
  where randomXs = take sampleSize $ randomRs (-0.5, 0.5) (mkStdGen seedX) :: [Double]
        randomYs = take sampleSize $ randomRs (-0.5, 0.5) (mkStdGen seedY) :: [Double]