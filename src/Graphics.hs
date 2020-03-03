{-# LANGUAGE OverloadedStrings #-}

module Graphics where

import           World
import Generation

import           HGamer3D
import qualified Data.Text as T
import           Control.Concurrent
import           Control.Monad


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
    ctMaterial #: matBrownStone,
    ctScale #: Vec3 1.0 1.0 1.0,
    ctPosition #:  Vec3 x y z,
    ctOrientation #: unitU
    ]
  return eCube

--allCubes :: Int -> [(Integer, Integer, Integer)] -> Picture
allCubes hg3d cubePositions = do
    cubes <- mapM (\pos -> makeCube hg3d pos) cubePositions
    return cubes

gameLogic hg3d = do    
    let chunk = sampleChunk
    let chunkIndex = (0, 0) 
    let allPositions = map ((posMap fromIntegral) . (absoluteBlockPosition chunkIndex)) chunk
     
    -- create camera
    cam <- newE hg3d [
                ctCamera #: FullViewCamera,
                ctPosition #: Vec3 1 1 (-30.0),
                ctLight #: Light PointLight 1.0 1000.0 1.0,
                ctOrientation #: unitU
                ]

    -- make cubes
    cubes <- allCubes hg3d allPositions

    -- create key
--    "wK" <: [
--        ctKeyEvent #: NoKeyEvent
--        ]

    -- registerCallback hg3d wK ctKeyEvent (\evt -> do
    --                                                 updateC cam ctOrientation (\u -> (rotU vec3X 0.05) .*. u)
    --                                                 return ()
    --                                              )

    return ()
