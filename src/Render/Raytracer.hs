module Render.Raytracer where

import Geometry.Object
import Geometry.Vector
import Math.SceneParams

-- generates the viewing ray for a given pixel coord
computeRay :: Scalar -> Scalar -> Ray
computeRay i j = ray
  where ray = Ray e (s <-> e)
        s = e <+> (mult u uCoord) <+> (mult v vCoord) <-> (mult w d)
        uCoord = l + (r-l) * (i+0.5)/(fromIntegral width)
        vCoord = b + (t-b) * (j+0.5)/(fromIntegral height)
