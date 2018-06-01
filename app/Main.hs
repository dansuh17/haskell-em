module Main where

import Lib
import DataSet (readData)

main :: IO ()
main = do
    genData <- readData "src/data.csv" "src/label.csv"
    putStrLn $ show genData
