{-# LANGUAGE ScopedTypeVariables #-}
module EmCoinState where

import Numeric.LinearAlgebra (Vector, R, Matrix, vector, fromList, toList, fromRows, (<.>), size, toRows, fromColumns, (><), (<>))
import Control.Monad.Trans.State ()

-- total number of throws
totalThrow :: R
totalThrow = 10

-- initial parameters
theta :: Vector R  -- vector of real values - R is just an alias of Double
theta = vector [0.1, 0.3]

-- observed data represented by the number of heads in 10 coin-flips
observed :: Vector R
observed = vector [5, 5, 7, 8, 9, 8, 4, 5, 7, 2, 3, 4, 4, 4, 8, 8, 9, 8]

-- test the validity of observed data - should be less than 10
testObserved :: Vector R -> Bool
testObserved = foldl (\acc hd -> ((hd <= 10) && acc)) True . toList

-- probability of coin being generated
probCoin :: Vector R
probCoin = vector [0.5, 0.5]

-- binomial probability
binomProb :: Matrix R -> Matrix R -> Matrix R -> Matrix R
binomProb heads tails bias = (bias ** heads) * ((1 - bias) ** tails)

-- eventProb :: R -> R -> R -> R
-- eventProb numhead bias coinprob = coinprob * binomProb numhead (totalThrow - numhead) bias

-- probability that an event will happen = P(coin) * P(num_heads | coin)
eventProb :: Vector R -> Vector R -> Vector R -> Matrix R
eventProb numheads headBias coinprobs = coinprobMat * binomProb headMat tailMat headBiasMat
  where
    numCoins = size headBias
    numSamples = size numheads
    headMat :: Matrix R = fromRows $ replicate numCoins numheads
    tailMat = 10 - headMat
    headBiasMat :: Matrix R = fromColumns $ replicate numSamples headBias
    coinprobMat :: Matrix R = fromColumns $ replicate numSamples coinprobs

-- calculate the sum over colums
-- this is like: map sum [column_vectors]
columnSum :: Matrix R -> Matrix R
columnSum x = (1><dim) (repeat 1) <> x  -- matmul with size (1 x colsize) = sum over columns
  where
    dim :: Int = fst $ size x  -- dimension of axis 0 (size of column)

-- normalize a matrix across column (axis 1)
columnNormalize :: Matrix R -> Matrix R
columnNormalize x = x / colSum
  where
    colSum = columnSum x

-- E-step == expected values of coins
-- each row (index axis 0) => examples
-- each colums (index axis 1) => coin's expected values (per example)
--
-- P(coin_i | observed, theta) = P(observed | coin_i, theta) * P(coin_i) / sum_over_k( P(observed | coin_k, theta) * P(coin_k) )
-- prod_over_x ( binom x_head (10 - x_head) coin_i_bias * 0.5 )
-- resulting size : (num_coins, num_examples)
coinExpected :: Vector R -> Vector R -> Vector R -> Matrix R
coinExpected heads biases coinprobs = columnNormalize eventp
  where
    eventp = eventProb heads biases coinprobs

-- M-step = calculate updated theta
-- new_theta_coin = sum (weighted heads) / sum (weighted total_throws)
thetaUpdated :: Vector R -> Matrix R -> Vector R
thetaUpdated obsvd exps = fromList $
  map (\weightVec ->  -- expeced values for coin
          weightVec <.> obsvd / weightVec <.> totalThrows)  -- weighted sum = dot product
      sampleExpected
  where
    -- Expected values for samples per coin. If there are two coins, length sampleExpected == 2
    sampleExpected :: [Vector R] = toRows exps  -- becomes a list of : [expected values of all samples for coin]
    totalThrows :: Vector R = vector $ replicate numSamps totalThrow
    numSamps :: Int = size $ head sampleExpected

-- returns all thetas through the iteration
emIterate :: [Vector R] -> Vector R -> Vector R -> Int -> [Vector R]
emIterate prevThetas obsvd coinprobs numIter = if numIter == 0 then prevThetas else
    emIterate (newTheta:prevThetas) obsvd coinprobs (numIter - 1)
  where
    coinExp = coinExpected obsvd prevTheta coinprobs  -- E step
    prevTheta = head prevThetas
    newTheta = thetaUpdated obsvd coinExp  -- M step
