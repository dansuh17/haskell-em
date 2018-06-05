{-# LANGUAGE ScopedTypeVariables #-}

module EM where

import System.Random
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.List.Split (chunksOf)

import DataSet (GeneratedData, sample)
import Numeric.LinearAlgebra (Vector, R, fromList, Matrix, inv, (#>), dot, det, size, (><))
import Data.Random.Distribution.MultivariateNormal ()

-- make EM algorithm
-- E-step
-- M-step
class EMTrainer a where
    eStep :: a -> a
    mStep :: a -> a

newtype EMClassifier = EMClassifier GeneratedData

instance EMTrainer EMClassifier where

-- generate random centroids
-- TODO: MUST RUN SEPARATE RANDOM PROCESSES
randomCentroids :: Int -> Int -> Double -> Double -> IO [Vector R]
randomCentroids numCenter dim lb hb = do
    stdGen <- getStdGen
    let randomStream :: [R] = randomRs (lb, hb) stdGen
        nums = take (dim * numCenter) randomStream
        chunks = chunksOf dim nums
    return $ map fromList chunks
-- randomCentroids numCenter dim lb hb = do
--     stdGen <- getStdGen  -- initial random generator
--     fst <$> randomCentroidRec ([], stdGen) numCenter
--   where
--     randomCentroidRec :: RandomGen g => ([Vector R], g) -> Int -> IO ([Vector R], g)
--     randomCentroidRec state@(accum, gen) remnum =
--       if remnum == 0
--       then return state  -- complete the recursion
--       else do
--         (centroid, nextgen) <- randomCentroid gen dim lb hb
--         let centroidVec = fromList centroid
--         -- pass the next generator state and accumulated values
--         randomCentroidRec (centroidVec:accum, nextgen) (remnum - 1)

-- initialize all covariance to 1.0
-- TODO: MUST RUN SEPARATE RANDOM PROCESSES
randomVariance :: Int -> Int -> IO [Matrix R]
randomVariance numcls dim = do
    stdGen <- getStdGen
    let randomStream :: [R] = randomRs (10, 20) stdGen
        totalNum = dim * dim * numcls
        nums = take totalNum randomStream
        chunks = chunksOf (dim * dim) nums
    return $ map (dim><dim) chunks
-- randomVariance numcls dim = do
--     stdGen <- getStdGen
--     fst <$> randomVarRec ([], stdGen) numcls
--   where
--     randomVarRec :: RandomGen g => ([Matrix R], g) -> Int -> IO ([Matrix R], g)
--     randomVarRec state@(accum, gen) remnum =
--         if remnum == 0
--         then return state
--         else do
--             let (rcs, nextgen) = randomR (0.5, 1.5) gen
--                 mat = (dim><dim) rcs
--             randomVarRec (mat:accum, nextgen) (remnum - 1)

-- caculate the bivariate gaussian probability of a sample data
sampleGaussProb :: Vector R -> Matrix R -> Vector R -> Double
sampleGaussProb mean cov samp = baseterm * expTerm
  where
    diff :: Vector R = samp - mean
    covInv :: Matrix R = inv cov
    expTerm :: Double = exp (-0.5 * (diff `dot` (covInv #> diff)))  -- the exp term
    covDet = det cov
    baseterm :: Double = 1 / sqrt (((2 * pi) ^ dim) * covDet)  -- the multiplied term
    dim :: Int = size mean

-- TODO: test required
argmax :: Ord a => [a] -> Int
argmax = fst . maximumBy (comparing snd) . zip [0..]

-- classify samples based on current parameters
classify :: GeneratedData -> [(Vector R, Matrix R)] -> [[Vector R]]
classify gendata params =
    foldl
      (\arr samp ->
            let idx = argmax $ map ($ samp) probCalculator  -- find the idx that gives most probability
            in take idx arr ++ (samp:(arr !! idx)):drop (idx + 1) arr)  -- assign the sample
      (replicate numClasses [])
      samples
  where
    samples = sample gendata  -- generated data samples
    numClasses :: Int = length params
    -- calculates the probability of a sample
    probCalculator :: [Vector R -> Double]
    probCalculator = [sampleGaussProb m c | (m, c) <- params]

classProbability :: GeneratedData -> [(Vector R, Matrix R)] -> [[Double]]
classProbability gendata params = [map (sampleGaussProb m c) samples | (m, c) <- params]
  where
    samples = sample gendata

-- initial classification result
classifyInit :: GeneratedData -> IO [[Vector R]]
classifyInit gendata = do
    randCents <- randomCentroids nc dim lb hb
    randVars <- randomVariance nc dim
    return $ classify gendata (zip randCents randVars)
  where
    nc = 5 -- number of classes
    dim = 2  -- dimension of data
    lb = -10.0
    hb = 10.0
