module Main where

import HGamer3D
import Graphics

-- main program, running gameLogic with standard configuration
main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
