{-# LANGUAGE
    DeriveGeneric
  , DerivingStrategies
  , TemplateHaskell
  , StandaloneDeriving
  , UndecidableInstances
#-}

import Generics.Constraints
import GHC.Generics
import Test.HUnit

newtype T a = T (a Int)
    deriving stock Generic

makeDeriving ''Eq   ''T
makeDeriving ''Ord  ''T
makeDeriving ''Show ''T

main :: IO Counts
main = runTestTT $ test
  [ assert (T [5] > T [3])
  ]
