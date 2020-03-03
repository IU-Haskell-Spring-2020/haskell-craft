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
makeCube :: HG3D -> (Integer, Integer, Integer) -> IO Entity
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

--gameLogic hg3d = do
--  es <- newET hg3d [
--    "eK" <: [
--        ctKeyEvent #: NoKeyEvent
--        ],
--
--    "eS" <: [
--        ctScreenModeEvent #: ScreenModeEvent 0 0 False False
--        ],
--
--     "txt" <: [
--        ctStaticText #: "",
--        ctScreenRect #: ScreenRect 10 100 200 35
--              ],
--
--     "txt2" <: [
--        ctStaticText #: "",
--        ctScreenRect #: ScreenRect 10 150 200 35
--              ]
--     ]
--
--  registerCallback hg3d (es # "eK") ctKeyEvent (\evt -> do
--                                                   setC (es # "txt") ctStaticText ("key event")
--                                                   setC (es # "txt2") ctStaticText (T.pack (show evt))
--                                                   return ()
--                                               )
--
--  registerCallback hg3d (es # "eS") ctScreenModeEvent (\evt -> do
--                                                   setC (es # "txt") ctStaticText ("screen mode event")
--                                                   setC (es # "txt2") ctStaticText (T.pack (show evt))
--                                                   return ()
--                                                 )
--
--  return es

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
    
    -- connect handlers
    es <- newET hg3d 
      [
        "wK" <: 
          [
            ctKeyEvent #: NoKeyEvent
          ]
      ]
    
    registerCallback hg3d (es # "wK") ctKeyEvent (\evt -> do
        updateC cam ctOrientation (\u -> (rotU vec3X 0.005) .*. u)
        return ()
      )

    return ()
