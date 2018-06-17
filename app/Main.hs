module Main where

import EmCoinMat (emIterate, theta, observed, probCoin, testObserved, eventProb, columnSum, coinExpected)
import EmCoinState (emStepper, emIter, initEm)
import Numeric.LinearAlgebra (size, tr', vector)
import Control.Monad.Trans.State (runState)

main :: IO ()
main = do
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
    print $ runState (initEm (vector [0.1, 0.3]) >> emIter 10) $ vector []
