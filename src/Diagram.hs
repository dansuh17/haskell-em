module Diagram where

import qualified Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Backend.Diagrams

import Data.Random.Distribution.MultivariateNormal

import qualified Data.Random as R
import Data.Random.Source.PureMT
import Control.Monad.State
import qualified Numeric.LinearAlgebra.HMatrix as LA

nSamples :: Int
nSamples = 10000

sigma1, sigma2, rho :: Double
sigma1 = 3.0
sigma2 = 1.0
rho = 0.5

singleSample :: R.RVarT (State PureMT) (LA.Vector Double)
singleSample = R.sample $ Normal (LA.fromList [0.0, 0.0])
               (LA.sym $ (2 LA.>< 2) [ sigma1, rho * sigma1 * sigma2
                                     , rho * sigma1 * sigma2, sigma2])

multiSamples :: [LA.Vector Double]
multiSamples = evalState (replicateM nSamples $ R.sample singleSample) (pureMT 3)
pts = map (f . LA.toList) multiSamples
  where
    f [x, y] = (x, y)
    f _      = error "Only pairs for this chart"



-- chartPoint pointVals n = C.toRenderable layout
--   where
--     fitted = C.plot_points_values .~ pointVals
--               $ C.plot_points_style  . C.point_color .~ opaque red
--               $ C.plot_points_title .~ "Sample"
--               $ def
--     layout = C.layout_title .~ "Sampling Bivariate Normal (" ++ (show n) ++ " samples)"
--            $ C.layout_y_axis . C.laxis_generate .~ C.scaledAxis def (-3,3)
--            $ C.layout_x_axis . C.laxis_generate .~ C.scaledAxis def (-3,3)
--
--            $ C.layout_plots .~ [C.toPlot fitted]
--            $ def
--
-- diagM = do
--   denv <- defaultEnv C.vectorAlignmentFns 600 500
--   return $ fst $ runBackend denv (C.render (chartPoint pts nSamples) (500, 500))
