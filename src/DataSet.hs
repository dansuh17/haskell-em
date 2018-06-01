module DataSet where

import Numeric.LinearAlgebra (Vector, Matrix, R, fromList)

data GeneratedData = GenData
    { sample :: [Vector R]
    , label :: Vector R
    } deriving (Show)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c bs = takeWhile (/= c) bs : splitOn c remainder
  where
    remainder = if null dropped then [] else tail dropped
    dropped = dropWhile (/= c) bs

-- read labels
labelFromFile :: String -> IO (Vector R)
labelFromFile fname = fromList . map read . splitOn '\n' <$> readFile fname

-- data points
sampleFromFile :: String -> IO [Vector R]
sampleFromFile fname = -- splitOn '\n'  -- split by lines
    map fromList . map (map read . splitOn ',') . splitOn '\n' <$> readFile fname

-- read both label data and sample data and create a dataset
readData :: String -> String -> IO GeneratedData
readData datafile labelfile = do
  samp <- sampleFromFile datafile
  lab <- labelFromFile labelfile
  return $ GenData samp lab

drawData :: GenData -> IO ()
drawData = return
