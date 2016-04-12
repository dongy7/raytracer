module Render.Setup where

import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import Graphics.UI.GLUT hiding (Sphere, Plane)
import Geometry.Object
import Render.Antialias

data State = State { zoomFactor :: IORef GLfloat }
type Image = PixelData (Color3 GLfloat)

fstSphere :: Surface
fstSphere = Sphere 1 (-4, 0, -7) (Material (0.2, 0, 0) (1, 0, 0) (0, 0, 0) 0 0)

sndSphere :: Surface
sndSphere = Sphere 2 (0, 0, -7) (Material (0, 0.2, 0) (0, 0.5, 0) (0.5, 0.5, 0.5) 32 0)

thdSphere :: Surface
thdSphere = Sphere 1 (4, 0, -7) (Material (0, 0, 0.2) (0, 0, 1) (0, 0, 0) 0 0.8)

plane :: Surface
plane = Plane (0, 1, 0) (0, -2, 0) (Material (0.2, 0.2, 0.2) (1, 1, 1) (0, 0, 0) 0 0.5)

scene :: Scene
scene = [fstSphere, sndSphere, thdSphere, plane]

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

rayTrace :: Size -> GLsizei ->  IO Image
rayTrace (Size w h) n =
   fmap (PixelData RGB Float) $
      newArray [ c |
                 i <- [ 0 .. w - 1 ],
                 j <- [ 0 .. h - 1 ],
                 let c = convertToFloatColor $ boxFilter scene (fromIntegral j) (fromIntegral i) ]

createShadedImage :: IO Image
createShadedImage = do
                         clearColor $= Color4 0 0 0 0
                         shadeModel $= Flat
                         rowAlignment Unpack $= 1
                         rayTrace sceneImageSize 0x8
