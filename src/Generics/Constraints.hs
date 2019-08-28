{-# LANGUAGE
    PolyKinds
  , TypeFamilies
  , ConstraintKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableSuperClasses
  #-}
module Generics.Constraints (Constraints) where

import Data.Kind (Constraint)
import qualified Generics.Constraints.Internal as I

-- | Constraint-level 'duplicate', of kind @(k -> Constraint) -> k -> k -> Constraint@.
class (c a, a ~ b) => D (c :: k -> Constraint) a b
instance (c a, a ~ b) => D c a b

type Constraints t c = I.Constraints t t (D c)
