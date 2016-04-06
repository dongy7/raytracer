module Render.Setup where


import Data.Bits ( (.&.) )
import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Geometry
import Raytracer

data State = State { zoomFactor :: IORef GLfloat }
type Image = PixelData (Color3 GLfloat)

scene :: Scene
scene = [Geometry.Sphere 1 (-4, 0, -7) (Material (0.2, 0, 0) (1, 0, 0) (0, 0, 0) 0),
         Geometry.Sphere 2 (0, 0, -7) (Material (0, 0.2, 0) (0, 0.5, 0) (0.5, 0.5, 0.5) 32),
         Geometry.Sphere 1 (4, 0, -7) (Material (0, 0, 0.2) (0, 0, 1) (0, 0, 0) 0),
         Geometry.Plane (0, 1, 0) (0, -2, 0) (Material (0.2, 0.2, 0.2) (1, 1, 1) (0, 0, 0) 0)]

makeState :: IO State
makeState = do
   z <- newIORef 1
   return $ State { zoomFactor = z }

sceneImageSize :: Size
sceneImageSize = Size 512 512

display ::  Image -> DisplayCallback
display pixelData = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 0 0)
   drawPixels sceneImageSize pixelData
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   matrixMode $= Modelview 0
   loadIdentity
