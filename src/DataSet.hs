module DataSet where

import Numeric.LinearAlgebra (Vector, R, fromList)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c bs = takeWhile (/= c) bs : (splitOn c remainder)
  where
    remainder = tail $ dropWhile (/= c) bs

byteFile :: String -> IO (Vector R)
byteFile fname = fromList <$> map read <$> splitOn '\n' <$> readFile fname
