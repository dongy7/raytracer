module Main where

import Render.Setup
import Graphics.UI.GLUT

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 512 512
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   state <- makeState
   sceneImage <- createShadedImage
   displayCallback $= display sceneImage
   reshapeCallback $= Just reshape
   mainLoop
