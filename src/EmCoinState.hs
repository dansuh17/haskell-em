{-# LANGUAGE ScopedTypeVariables #-}
module EmCoinState where

import Control.Monad.Trans.State (State, put, state, get)
import Control.Monad (replicateM)
import Numeric.LinearAlgebra
    (Vector
    , R
    , Matrix
    , vector
    , fromList
    , toList
    , fromRows
    , (<.>)  -- dot product (vector)
    , (><)  -- matrix formation
    , (<>)  -- matmul
    , size
    , toRows
    , fromColumns
    )

-- total number of throws
totalThrow :: R
totalThrow = 10

-- initial parameters
-- initial bias of two coins giving heads
initParam :: Params  -- vector of real values - R is just an alias of Double
initParam = vector [0.4, 0.6]

-- observed data represented by the number of heads in 10 coin-flips
headObserved :: Vector R
headObserved = vector [2, 9, 7, 8, 9, 8, 3, 3, 7, 2]

-- test the validity of observed data - should be less than 10
testObserved :: Vector R -> Bool
testObserved = foldl (\acc hd -> ((hd <= 10) && acc)) True . toList

-- probability of coin being generated
probCoin :: Vector R
probCoin = vector [0.5, 0.5]

-- define type parameters
type Params = Vector R

-- binomial probability
binomProb :: Matrix R -> Matrix R -> Matrix R -> Matrix R
binomProb hs tails bs = (bs ** hs) * ((1 - bs) ** tails)

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
-- each columns (index axis 1) => coin's expected values (per example)

-- P(coin_i | observed, theta) = P(observed | coin_i, theta) * P(coin_i) / sum_over_k( P(observed | coin_k, theta) * P(coin_k) )
-- prod_over_x ( binom x_head (10 - x_head) coin_i_bias * 0.5 )
-- resulting size : (num_coins, num_examples)
coinProbEst :: Vector R -> Vector R -> State Params (Matrix R)
coinProbEst heads coinProbs = state $ \params ->
  (columnNormalize (eventProb heads params coinProbs), params)  -- doesn't modify the state

-- M-step = calculate updated theta
-- new_theta_coin = sum (weighted heads) / sum (weighted total_throws)
updateParams :: Vector R -> Matrix R -> State Params ()
updateParams heads coinExps = state $ const ((), calcUpdatedParams heads coinExps)

-- calculate the next parameters
calcUpdatedParams :: Vector R -> Matrix R -> Vector R
calcUpdatedParams obsvd exps = fromList $
  map (\weightVec ->  -- expeced values for coin
          weightVec <.> obsvd / weightVec <.> totalThrows)  -- weighted sum = dot product
      sampleExpected
  where
    -- Expected values for samples per coin. If there are two coins, length sampleExpected == 2
    sampleExpected :: [Vector R] = toRows exps  -- becomes a list of : [expected values of all samples for coin]
    totalThrows :: Vector R = vector $ replicate numSamps totalThrow
    numSamps :: Int = size $ head sampleExpected

-- initial coin bias values
initEm :: Vector R -> State Params ()
initEm = put

-- state contains the parameters.
-- TODO : make state processing function output a log-likelihood + current state
emStep :: Vector R -> Vector R -> State Params ()
emStep heads coinProbs = coinProbEst heads coinProbs >>= updateParams heads

-- one stepper state for EM
-- after the step, return the current state
emStepper :: State Params Params
emStepper = emStep headObserved probCoin >> get

-- iterate EM algorithm multiple times and collect intermediate params
emIter :: Int -> State Params [Params]
emIter numIter = replicateM numIter emStepper
