{-# LANGUAGE ScopedTypeVariables #-}

module EM where

import System.Random
import Data.List (maximumBy)
import Data.Ord (comparing)

import DataSet (GeneratedData, sample)
import Numeric.LinearAlgebra (Vector, R, fromList, Matrix, inv, (#>), dot, det, size, (><))

-- make EM algorithm
-- E-step
-- M-step
class EMTrainer a where
    eStep :: a -> a
    mStep :: a -> a

newtype EMClassifier = EMClassifier GeneratedData

instance EMTrainer EMClassifier where

-- generate a random centroid having 'dim' dimension
randomCentroid :: Int -> Double -> Double -> IO [Double]
randomCentroid dim lowBound highBound = do
    stdGen <- getStdGen
    let rcs = randomRs (lowBound, highBound) stdGen
    return $ take dim rcs

-- generate random cnetroids
-- MUST RUN SEPARATE RANDOM PROCESSES
randomCentroids :: Int -> Int -> Double -> Double -> IO [Vector R]
randomCentroids numCenter dim lb hb = mapM (fromList <$>) centroids
  where
    centroids = replicate numCenter $ randomCentroid dim lb hb

-- initialize all covariance to 1.0
randomVariance :: Int -> Int -> IO [Matrix R]
randomVariance numcls dim = do
    stdGen <- getStdGen
    let rcs = randomRs (0.5, 1.5) stdGen
    return $ replicate numcls $ (dim><dim) rcs

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
