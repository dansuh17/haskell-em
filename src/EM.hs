module EM where

import DataSet (GeneratedData(GenData))

-- make EM algorithm
-- E-step
-- M-step
class EMTrainer a where
    eStep :: a -> a
    mStep :: a -> a

newtype EMClassifier = EMClassifier GeneratedData

instance EMTrainer EMClassifier where

