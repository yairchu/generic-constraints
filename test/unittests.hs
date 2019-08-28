{-# LANGUAGE DeriveGeneric #-}

import Data.Functor.Contravariant
import Data.Functor.Identity
import GHC.Generics
import Test.HUnit
import Generics.Constraints

data T a = T0 | T1 a | T2 a a deriving (Eq, Show, Generic, Generic1)
data Pair a = Pair a a deriving (Eq, Show, Generic, Generic1)

main = runTestTT $ test
  [ (pure () :: IO ())
  ]
