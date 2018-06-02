module Main where

import DataSet (readData)
import Numeric.LinearAlgebra (meanCov, fromRows, fromList)

main :: IO ()
main = do
    genData <- readData "src/data.csv" "src/label.csv"
    print genData

    let
      (mean, cov) = meanCov (fromRows $ map fromList [[1, 2], [1, 2], [0.9, 1.9]])
    print mean
    print cov
