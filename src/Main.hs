module Main where

import Data.Bits ( (.&.) )
import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

