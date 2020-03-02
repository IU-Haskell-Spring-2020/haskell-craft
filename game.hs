{-# LANGUAGE OverloadedStrings #-}
import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad


-- names of built-in materials (we can use them, but better to use stone.png from minecraft)
mats = [
    matOrangeCrossMetal,
    matCrossMetal,
    matCrossMetalBlue,
    matMetal,
    matMetalZigZag,
    matMetalBumps,
    matFishEye,
    matMetalOrnament,
    matMetalScratch,
    matMetalLine,
    matGreenGrass,
    matBrownGrass,
    matGreyGrass,
    matSand,
    matRedRock,
    matBlackRock,
    matBrownStone,
    matStoneMetalWall,
    matCoalWall,
    matBrickWallGray,
    matBrickWallRed,
    matTilesOrange,
    matWoodTiles,
    matColourTiles,
    matBlackTiles
  ]

makeCube hg3d (x,y,z) = do
  eCube <- newE hg3d [
    ctGeometry #: ShapeGeometry Cube,
    ctMaterial #: matSand,
    ctScale #: Vec3 1.0 1.0 1.0,
    ctPosition #:  Vec3 x y z,
    ctOrientation #: unitU
    ]
  return eCube


allCubes hg3d cubePositions = do
    cubes <- mapM (\pos -> makeCube hg3d pos) cubePositions
    return cubes

-- You know what to do
getPositions :: [(Float, Float, Float)]
getPositions = [(3,0,0),(0,3,0),(0,0,3)]

gameLogic hg3d = do
    -- create camera
    cam <- newE hg3d [
                ctCamera #: FullViewCamera,
                ctPosition #: Vec3 1 0 (-30.0),
                ctLight #: Light PointLight 1.0 1000.0 1.0,
                ctOrientation #: unitU
                ]

    -- make cubes
    cubes <- allCubes hg3d getPositions

    -- create key
    "wK" <: [
        ctKeyEvent #: NoKeyEvent
        ]

    -- registerCallback hg3d wK ctKeyEvent (\evt -> do
    --                                                 updateC cam ctOrientation (\u -> (rotU vec3X 0.05) .*. u)
    --                                                 return ()
    --                                              ) 

    return ()


-- main program, running gameLogic with standard configuration
main = do
    runGame standardGraphics3DConfig gameLogic  (msecT 20)
    return ()
