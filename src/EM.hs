{-# LANGUAGE ScopedTypeVariables #-}

module EM where

import System.Random
import Data.List (maximumBy)
import Data.Ord (comparing)

import DataSet (GeneratedData, sample)
import Numeric.LinearAlgebra (Vector, R, fromList, Matrix, inv, (#>), dot, det, size)

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
randomCentroids :: Int -> Int -> Double -> Double -> IO [Vector R]
randomCentroids numCenter dim lb hb = mapM (fromList <$>) centroids
  where
    centroids = replicate numCenter $ randomCentroid dim lb hb

-- initialize variance to 1.0
initVariance :: Int -> [Double]
initVariance = flip replicate 1.0

-- caculate the bivariate gaussian probability of a sample data
sampleGaussProb :: Vector R -> Matrix R -> Vector R -> Double
sampleGaussProb mean cov samp = baseterm * expTerm
  where
    diff = samp - mean
    covInv = inv cov
    expTerm = exp (0.5 * (diff `dot` (covInv #> diff)))  -- the exp term
    covDet = det cov
    baseterm = 1 / ((sqrt (2 * pi) ^ dim) * covDet)  -- the multiplied term
    dim = size mean

argmax :: Ord a => [a] -> Int
argmax = fst . maximumBy (comparing snd) . zip [0..]

classify :: GeneratedData -> [(Vector R, Matrix R)] -> [[Vector R]]
classify gendata params =
    foldl
      (\arr samp ->
            let idx = argmax $ map ($ samp) probCalculator
            in take idx arr ++ (samp:(arr !! idx)):drop (idx + 1) arr)
      (replicate numClasses [])
      samples
  where
    samples = sample gendata
    numClasses :: Int = length params
    probCalculator :: [Vector R -> Double]
    probCalculator = [sampleGaussProb m c | (m, c) <- params]
