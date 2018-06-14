{-# LANGUAGE ScopedTypeVariables #-}
module EmCoinState where

import Numeric.LinearAlgebra (Vector, R, Matrix, vector, fromList, toList, fromRows, toRows, tr')
import Control.Monad.Trans.State ()

-- initial parameters
theta :: Vector R  -- vector of real values - R is just an alias of Double
theta = vector [0.1, 0.3]

-- observed data - (head, tail)
observed :: Vector (R, R)
observed = fromList [(5, 5), (5, 5), (7, 3), (8, 2), (9, 1), (8, 2),
                     (4, 6), (5, 5), (7, 3), (2, 8), (3, 7), (4, 6)]

-- test the validity of observed data
testObserved :: Vector (R, R) -> Bool
testObserved = foldl (\acc (hd, tl) -> ((hd + tl == 10) && acc)) True . toList

-- probability of coin being generated
probCoin :: Vector R
probCoin = vector [0.5, 0.5]

-- binomial probability
binomProb :: R -> R -> R -> R
binomProb numHeads numTails bias = (bias ^ numHeads) * ((1 - bias) ^ numTails)

-- P(coin_i | observed, theta) = P(observed | coin_i, theta) * P(coin_i) / sum_over_k( P(observed | coin_k, theta) * P(coin_k) )
-- prod_over_x ( binom x_head (10 - x_head) coin_i_bias * 0.5 )

-- probability that an event will happen
eventProb :: (R, R) -> R -> R -> R
eventProb (h, t) bias coinprob = coinprob * binomProb h t bias

allExpected :: Vector (R, R) -> Vector R -> Vector R -> [[R]]
allExpected observ biases coinprobs =
    map (\obs ->
            [eventProb obs b p | (b, p) <- biasprobs ])
        (toList observ)
  where
    biasprobs = zip (toList biases) (toList coinprobs)

normalize :: (Num a, Fractional a) => [a] -> [a]
normalize as = map (/ total) as
  where
    total = sum as

-- expected values of coins == E-Step
-- each row (index axis 0) => examples
-- each colums (index axis 1) => coin's expected values (per example)
coinExpected :: Vector (R, R) -> Vector R -> Vector R -> Matrix R  -- [[Double]]
coinExpected obsvd biases coinprobs = fromRows $ map (fromList . normalize) allexp
  where
    allexp = allExpected obsvd biases coinprobs

ciks :: Matrix R
ciks = coinExpected observed theta probCoin

-- TODO: expand to multiple coins
weightedSum :: Vector R -> Vector R -> R
weightedSum x w = sum $ zipWith (*) (toList x) (toList w)

-- expected values (probabilities) per coin
-- changes row-baesed 'coinExpected' to column-based 'sampleExpectedValues'
sampleExpectedValues :: Matrix R -> Matrix R
-- sampleExpectedValues sampExps = fromRows [ map (!! coinidx) (toRows sampExps) | coinidx <- [0..(numCoins - 1)] ]
--   where
--     numCoins = length $ head (toRows sampExps)
sampleExpectedValues = tr'

-- M-step = calculate theta
-- sum (weighted heads) * sum (weighted total)
thetaUpdated :: Vector (R, R) -> Matrix R -> Vector R
thetaUpdated obsvd cik = fromList $
  map (\x ->
    weightedSum x heads / weightedSum x (fromList $ repeat 10.0))
    (toRows sampleExps)
  where
    heads :: Vector R = fromList $ map (fromIntegral . fst) (toList obsvd)
    -- expected values per coin. if there are two coins, length sampleExps == 2
    sampleExps :: Matrix R = sampleExpectedValues cik

-- updated prime numbers
thetaPrime :: Vector R
thetaPrime = thetaUpdated observed ciks

-- returns all thetas through the iteration
emIterate :: [Vector R] -> Vector (R, R) -> Vector R -> Int -> [Vector R]
emIterate prevThetas obsvd coinprobs numIter = if numIter == 0 then prevThetas else
    emIterate (newTheta:prevThetas) obsvd coinprobs (numIter - 1)
  where
    coinExp = coinExpected obsvd prevTheta coinprobs  -- E step
    prevTheta = head prevThetas
    newTheta = thetaUpdated obsvd coinExp  -- M step
