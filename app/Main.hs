module Main where

import EmCoinState (emIterate, theta, observed, probCoin, testObserved, eventProb, columnSum, coinExpected)
import Numeric.LinearAlgebra (size, tr')

main :: IO ()
main = do
    print $ testObserved observed
    print $ size observed
    print $ tr' $ eventProb observed theta probCoin
    print $ tr' $ columnSum $ eventProb observed theta probCoin
    print $ tr' $ coinExpected observed theta probCoin

    print $ emIterate [theta] observed probCoin 10
