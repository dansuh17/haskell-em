module Main where

import Numeric.LinearAlgebra (size, tr', vector)
import Control.Monad.Trans.State (runState, evalState)
import Control.Monad (mapM_)
import DataGen (samples)
import EmCoinMat (emIterate, theta, observed, probCoin, testObserved, eventProb, columnSum, coinExpected)
import EmCoinState (emStepper, emIter, initEm)

-- prints the step number along with the state
printIterNum :: Show a => (Int, a) -> IO ()
printIterNum (iter, state) = print $ "Step : " ++ show iter ++ "  Params: " ++ show state

main :: IO ()
main = do
    -- EM using hmatrix computations
    print "EM using no states"
    print $ testObserved observed
    print $ size observed
    print $ tr' $ eventProb observed theta probCoin
    print $ tr' $ columnSum $ eventProb observed theta probCoin
    print $ tr' $ coinExpected observed theta probCoin
    print $ emIterate [theta] observed probCoin 10

    -- start using stateful EM
    print "stateful EM"
    print $ runState emStepper $ vector [0.1, 0.3]
    print $ runState (emIter 4) $ vector [0.1, 0.3]  -- show the state after 4 iterations
    -- theta values after 10 iterations
    print $ fst . runState (initEm (vector [0.1, 0.3]) >> emIter 10) $ vector []
    -- using 'evalState' and pretty printing
    mapM_ printIterNum $ zip [1..] (evalState (initEm (vector [0.1, 0.3]) >> emIter 10) $ vector [])

    samps <- samples
    print samps
