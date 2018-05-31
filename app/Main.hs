module Main where

import Lib
import DataSet (byteFile)

main :: IO ()
main = do
    labelVec <- byteFile "src/label.csv"
    putStrLn $ show labelVec
