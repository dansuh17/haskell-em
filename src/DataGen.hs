{-# LANGUAGE ScopedTypeVariables #-}
module DataGen where

import Control.Monad (replicateM)
import qualified Data.Random as DR
import Data.Random.Distribution.Bernoulli (bernoulli)

-- throws the coin N times according to the bernoulli distribution
-- with probability given by 'bias'
throwNTimes :: Int -> Double -> DR.RVar [Double]
throwNTimes totalThrows bias = replicateM totalThrows (bernoulli bias)

-- split the list by n elements
takeByN :: Int -> [a] -> [[a]]
takeByN _ [] = []
takeByN n xs = take n xs:takeByN n (drop n xs)

-- count the number of heads (== 1) in the list
countHeads :: [Double] -> Int
countHeads samples = sum (filter (== 1) (map floor samples))

-- concatMapM :: (Monad f, Traversable t) => (a -> f [b]) -> t a = f [b]
concatMapM f l = fmap concat (mapM f l)

-- generates coin samples, a list of number of heads after 'totalThrows' throws
generateCoinSamples :: Int -> [(Double, Int)] -> IO [Int]
generateCoinSamples totalThrows biasNumSamps = do
    let throwRVars :: [DR.RVar [Double]] =
          [ throwNTimes (totalThrows * numSamps) bias
            | (bias, numSamps) <- biasNumSamps ]
    samples :: [Double] <- concatMapM (`DR.runRVar` DR.StdRandom) throwRVars
    let trials = takeByN totalThrows samples
    return $ map countHeads trials

-- [EXAMPLE USAGE] of sampling from a distribution
-- bias1 :: Double
-- bias1 = 0.8
--
-- samples :: IO [Double]
-- samples = DR.runRVar (replicateM 20 (bernoulli bias1)) DR.StdRandom
