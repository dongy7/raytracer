module Render.Antialias where

import Geometry.Vector
import Geometry.Object
import Math.SceneParams
import Render.Raytracer
import System.Random

boxFilter :: Scene -> Scalar -> Scalar -> Colour
boxFilter scene i j = avgColor
  where randomPixels = genRandomPixels i j
        shadings = map (computePixelPairColor scene) randomPixels
        avgColor = getAverageColor shadings

-- get average color from color list
getAverageColor :: [Colour] -> Colour
getAverageColor colors = avg
  where colorSum = foldl addColour (0, 0, 0) colors
        len = fromIntegral $ length colors
        reciprocal = 1/len
        avg = scaleColour colorSum reciprocal


-- generates random sample of pixels within the boundaries of a given pixel
genRandomPixels :: Scalar -> Scalar -> [(Scalar, Scalar)]
genRandomPixels i j = randomCoords
  where randomXs = take sampleSize $ randomRs (-0.5, 0.5) (mkStdGen seedX) :: [Double]
        randomYs = take sampleSize $ randomRs (-0.5, 0.5) (mkStdGen seedY) :: [Double]
        combined = zip randomXs randomYs
        ij = replicate sampleSize (i, j)
        randomCoords = zipWith addTuple combined ij


addTuple :: (Scalar, Scalar) -> (Scalar, Scalar) -> (Scalar, Scalar)
addTuple (x, y) (x', y') = (x+x', y+y')
