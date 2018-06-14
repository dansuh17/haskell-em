{-# LANGUAGE ScopedTypeVariables #-}
module EmCoin where

import Numeric.LinearAlgebra ()

-- initial parameters
theta :: [Double]
theta = [0.1, 0.3]

-- observed data - (head, tail)
observed :: [(Int, Int)]
observed = [(5, 5), (5, 5), (7, 3), (8, 2), (9, 1), (8, 2),
            (4, 6), (5, 5), (7, 3), (2, 8), (3, 7), (4, 6)]

-- test the validity of observed data
testObserved :: [(Int, Int)] -> Bool
testObserved = foldl (\acc (hd, tl) -> ((hd + tl == 10) && acc)) True

-- probability of coin being generated
probCoin :: [Double]
probCoin = [0.5, 0.5]

binomProb :: Int -> Int -> Double -> Double
binomProb numHeads numTails bias = (bias ^ numHeads) * ((1 - bias) ^ numTails)

-- P(coin_i | observed, theta) = P(observed | coin_i, theta) * P(coin_i) / sum_over_k( P(observed | coin_k, theta) * P(coin_k) )
-- prod_over_x ( binom x_head (10 - x_head) coin_i_bias * 0.5 )
eventProb :: (Int, Int) -> Double -> Double -> Double
eventProb (h, t) bias coinprob = coinprob * binomProb h t bias

allExpected :: [(Int, Int)] -> [Double] -> [Double] -> [[Double]]
allExpected observ biases coinprobs =
    map (\obs ->
            [eventProb obs b p | (b, p) <- biasprobs ])
        observ
  where
    biasprobs = zip biases coinprobs

normalize :: (Num a, Fractional a) => [a] -> [a]
normalize as = map (/ total) as
  where
    total = sum as

-- expected values of coins == E-Step
-- each row (index axis 0) => examples
-- each colums (index axis 1) => coin's expected values (per example)
coinExpected :: [(Int, Int)] -> [Double] -> [Double] -> [[Double]]
coinExpected obsvd biases coinprobs = map normalize allexp
  where
    allexp = allExpected obsvd biases coinprobs

ciks :: [[Double]]
ciks = coinExpected observed theta probCoin

-- TODO: expand to multiple coins
weightedSum :: [Double] -> [Double] -> Double
weightedSum x w = sum $ zipWith (*) x w

-- expected values (probabilities) per coin
-- changes row-baesed 'coinExpected' to column-based 'sampleExpectedValues'
sampleExpectedValues :: [[Double]] -> [[Double]]
sampleExpectedValues sampExps = [ map (!! coinidx) sampExps | coinidx <- [0..(numCoins - 1)] ]
  where
    numCoins = length $ head sampExps

-- M-step = calculate theta
-- sum (weighted heads) * sum (weighted total)
thetaUpdated :: [(Int, Int)] -> [[Double]] -> [Double]
thetaUpdated obsvd cik = map (\x ->
                                weightedSum x heads / weightedSum x (repeat 10.0))
                             sampleExps
  where
    heads :: [Double] = map (fromIntegral . fst) obsvd
    -- expected values per coin. if there are two coins, length sampleExps == 2
    sampleExps = sampleExpectedValues cik

-- updated prime numbers
thetaPrime :: [Double]
thetaPrime = thetaUpdated observed ciks

-- returns all thetas through the iteration
emIterate :: [[Double]] -> [(Int, Int)] -> [Double] -> Int -> [[Double]]
emIterate prevThetas obsvd coinprobs numIter = if numIter == 0 then prevThetas else
    emIterate (newTheta:prevThetas) obsvd coinprobs (numIter - 1)
  where
    coinExp :: [[Double]] = coinExpected obsvd prevTheta coinprobs  -- E step
    prevTheta :: [Double] = head prevThetas
    newTheta = thetaUpdated obsvd coinExp  -- M step
