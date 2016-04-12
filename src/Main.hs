module Main where

import Render.Setup
import Graphics.UI.GLUT
import Data.Time

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   start <- getCurrentTime
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 512 512
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   state <- makeState
   sceneImage <- createShadedImage
   displayCallback $= display sceneImage
   reshapeCallback $= Just reshape
   stop <- getCurrentTime
   putStr "Rendered in: "
   print $ (diffUTCTime stop start) 
   mainLoop
