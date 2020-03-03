module Generation (randomChunkPoint, generateChunk, sampleChunk) where

import           Constants
import           World
import           NoiseGen.Perlin
import           NoiseGen.Noise (Seed)
import           Data.Int
import           System.Random

-- Generate random point in a chunk
randomChunkPoint :: StdGen -> (RelativePosition, StdGen)
randomChunkPoint g = ((randX, randY, randZ), finalG)
  where
    (randX, g') = randomR (0, fromIntegral chunkSize) g
    (randY, g'') = randomR (0, fromIntegral maxYLevel) g'
    (randZ, finalG) = randomR (0, fromIntegral chunkSize) g''

-- | All points (X, Z) of a chunk
allChunkLevelPoints :: [(Int8, Int8)]
allChunkLevelPoints 
  = (,) <$> [0 .. (fromIntegral chunkSize - 1)] <*> [0 .. (fromIntegral chunkSize - 1)]

sampleChunk :: [Block]
sampleChunk = generateChunk testSeed Normal Mountains

testWorldType :: WorldType
testWorldType = Normal

testBiome :: Biome
testBiome = Plains

generateChunk :: Seed -> WorldType -> Biome -> [Block]
generateChunk _ Flat _          = flatBedrock ++ terrain
  where
    terrain = (\(x, z) y -> Block (x, z, y) Grass) <$> allChunkLevelPoints <*> [1, 2]
        
generateChunk seed Normal Plains = terrain
  where
    octaves = 5
    scale = 0.6
    persistence = 0.1
    perlinDist = Perlin seed octaves scale persistence
    threshold = 0.5
    
    calculateHeight x z = toHeight $ noiseValue perlinDist (x, z, fromIntegral seaLevel)
    toHeight noiseVal   = fromIntegral $ floor (noiseVal * 10) + seaLevel
    makeBlock blockType x z y = Block (x, z, y) blockType
    
    blocks = map (\(x, z) -> (x, z, calculateHeight (fromIntegral x) (fromIntegral z))) allChunkLevelPoints
    
    terrain = map (flip Block Stone) blocks

generateChunk seed Normal Mountains = terrain
  where
    octaves     = 4
    scale       = 1
    persistence = 0.1
    perlinDist  = Perlin seed octaves scale persistence
    threshold   = 0.5

    toHeight noiseVal   = fromIntegral $ floor (noiseVal * 10) + seaLevel
    calculateHeight x z = toHeight $ noiseValue perlinDist (fromIntegral x, fromIntegral z, fromIntegral seaLevel)
    makeBlock x z y = Block (x, z, y) Stone
    
    upperBlocks = map (\(x, z) -> (x, z, calculateHeight (fromIntegral x) (fromIntegral z))) allChunkLevelPoints    
    makeColumn (x, z, height) = zipWith3 makeBlock (repeat x) (repeat z) [1 .. height]
    
    terrain = map (flip Block Stone) upperBlocks

flatBedrock :: [Block]
flatBedrock = map (\(x, z) -> Block (x, z, 0) Bedrock) allChunkLevelPoints