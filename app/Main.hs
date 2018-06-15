module Main where

import EmCoinState (ciks, thetaPrime, emIterate, theta, observed, probCoin, testObserved)
import Numeric.LinearAlgebra (toList, toRows, size)

main :: IO ()
main = do
{-
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
    -}
    print $ testObserved observed
    print $ size observed
    print $ size ciks
    mapM_ print $ zip (toList observed) (toRows ciks)
    print thetaPrime  -- TODO: looping infinitely!

    print $ emIterate [theta] observed probCoin 10
{-
    let mult = multiSamples
        norm = Normal (fromList [0.0, 0.0]) (sym $ (2><2) [3.0, 0, 0, 3.0])
        probs = map (pdf norm) mult
        -}
    -- print probs
