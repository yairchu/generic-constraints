{-# LANGUAGE
    DeriveGeneric
  , StandaloneDeriving
  , UndecidableInstances
#-}

import Generics.Constraints
import GHC.Generics
import Test.HUnit

data T a = T (a Int)
    deriving Generic

deriving instance Constraints (T a) Eq   => Eq   (T a)
deriving instance Constraints (T a) Ord  => Ord  (T a)
deriving instance Constraints (T a) Show => Show (T a)

main = runTestTT $ test
  [ assert (T [5] > T [3])
  ]
