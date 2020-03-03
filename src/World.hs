module World where

import           Data.Int
import           Constants

data World = World
    {
      seed           :: Int,
      spawnBlock     :: RelativePosition,
      chunks         :: [Chunk],
      playerPosition :: AbsolutePosition,
      worldType      :: WorldType
    }

data WorldType
  = Flat
  | Normal
  deriving (Show)

data Biome
  = Plains
  | Mountains
  deriving (Eq, Show)

data Chunk = Chunk
    {
      chunkIndex :: ChunkIndex,
      terrain    :: [Block],
      biome      :: Biome
    }

data BlockType
  = Grass
  | Stone
  | Bedrock
  deriving (Show, Eq)

data Block = Block
  {
    position  :: RelativePosition, -- ^ x, y, and z coordinates of the block relative to the chunk
    blockType :: BlockType         -- ^ Type of the block
  }
  deriving (Show)

type ChunkIndex       = (Integer, Integer)
-- | X, Z, Y
type RelativePosition = (Int8, Int8, Int16)
type AbsolutePosition = (Integer, Integer, Integer)

posMap :: (Integer -> b) -> AbsolutePosition -> (b, b, b)
posMap f (x, z, y) = (f x, f z, f y)

maxGenerationYLevel :: Biome -> Int
maxGenerationYLevel Plains = 120
maxGenerationYLevel Mountains = 210

-- | Given starting position, current chunk, and the position in the current chunk,
-- calculate the absolute position (bottom corner)
toAbsolutePosition :: ChunkIndex -> RelativePosition -> AbsolutePosition
toAbsolutePosition (xIndex, zIndex) (xBlock, zBlock, yBlock) = (xOffset, zOffset, yOffset)
  where
    xOffset = fromIntegral xBlock + xIndex * fromIntegral chunkSize
    zOffset = fromIntegral zBlock + zIndex * fromIntegral chunkSize
    yOffset = fromIntegral yBlock

toRelativePosition :: AbsolutePosition -> (ChunkIndex, RelativePosition)
toRelativePosition (absX, absZ, absY) = ((chunkX, chunkZ), (fromIntegral blockX, fromIntegral blockZ, blockY))
  where
    (chunkX, blockX) = absX `divMod` (fromIntegral chunkSize)
    (chunkZ, blockZ) = absZ `divMod` (fromIntegral chunkSize)
    blockY = fromIntegral absY

absoluteBlockPosition :: ChunkIndex -> Block -> AbsolutePosition
absoluteBlockPosition chunkIndex = (toAbsolutePosition chunkIndex) . position