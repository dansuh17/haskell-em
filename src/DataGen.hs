module DataGen where

import Control.Monad (replicateM)
import qualified Data.Random as DR
-- import Data.Random.Distribution (sample)
import Data.Random.Distribution.Bernoulli (bernoulli)

bias1 :: Double
bias1 = 0.9

samples :: IO [Double]
samples = DR.runRVar (replicateM 20 (bernoulli bias1)) DR.StdRandom :: IO [Double]
