module Main where

import DataSet (readData)
import EM (classifyInit, randomVariance, randomCentroids, classProbability)
import Numeric.LinearAlgebra (meanCov, fromRows, fromList)

main :: IO ()
main = do
    genData <- readData "src/data.csv" "src/label.csv"
    print genData

    let
      (mean, cov) = meanCov (fromRows $ map fromList [[1, 2], [1, 2], [0.9, 1.9]])
    print mean
    print cov

    classified <- classifyInit genData
    print classified

    let nc = 5
        dim = 2
        lb = -10.0
        hb = 10.0
    randCents <- randomCentroids nc dim lb hb
    print randCents
    randVars <- randomVariance nc dim
    print randVars
    let probs = classProbability genData (zip randCents randVars)
    print probs
