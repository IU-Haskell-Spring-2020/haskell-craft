{-# LANGUAGE OverloadedStrings #-}

module Graphics where

import           World
import           Generation

import           HGamer3D
import qualified Data.Text as T
import           Control.Concurrent
import           Control.Monad

blockSize :: Float
blockSize = 5

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

--makeCube :: a -> AbsolutePosition -> b
makeCube hg3d (x, z, y) = do
  let (x', y', z') = posMap (\n -> fromIntegral n * blockSize) (x, y, z)
  eCube <- newE hg3d [
    ctGeometry    #: ShapeGeometry Cube,
    ctMaterial    #: matStoneMetalWall,
    ctScale       #: Vec3 blockSize blockSize blockSize,
    ctPosition    #: Vec3 x' z' y',
    ctOrientation #: unitU
    ]
  return eCube

--allCubes :: a -> [AbsolutePosition] -> b
allCubes hg3d cubePositions =
  do
    cubes <- mapM (\pos -> makeCube hg3d pos) cubePositions
    return cubes

updateCamera cam = do
  updateC cam ctOrientation (\c -> (rotU vec3X (-0.0005)) .*. c)
  updateC cam ctOrientation (\c -> (rotU vec3Y (0.0005)) .*. c)
--  updateC cam ctOrientation (\c -> (rotU vec3Y (-0.001)) .*. c)
  sleepFor (msecT 12)
  updateCamera cam


gameLogic hg3d = do
    let chunk = sampleChunk
    let chunkIndex = (0, 0)
    let allPositions = map (absoluteBlockPosition chunkIndex) chunk

    -- create camera
    cam <- newE hg3d
      [
        ctCamera      #: FullViewCamera,
        ctPosition    #: Vec3 50 50 160,
        ctLight       #: Light PointLight 1.0 1000.0 1.0,
        ctOrientation #: unitU
      ]

    -- make cubes
    cubes <- allCubes hg3d allPositions
    forkIO (updateCamera cam)
    -- create key
--    "wK" <: [
--        ctKeyEvent #: NoKeyEvent
--        ]

    -- registerCallback hg3d wK ctKeyEvent (\evt -> do
    --                                                 updateC cam ctOrientation (\u -> (rotU vec3X 0.05) .*. u)
    --                                                 return ()
    --                                              )

    return ()
