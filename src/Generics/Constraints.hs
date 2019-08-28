{-# LANGUAGE
    PolyKinds
  , TypeFamilies
  , TypeOperators
  , ConstraintKinds
  #-}
module Generics.Constraints (Constraints) where

import Data.Kind (Constraint)
import GHC.Generics

type family Constraints' (t :: * -> *) (c :: * -> Constraint) :: Constraint
type instance Constraints' V1 c = ()
type instance Constraints' U1 c = ()
type instance Constraints' (f :+: g) c = (Constraints' f c, Constraints' g c)
type instance Constraints' (f :*: g) c = (Constraints' f c, Constraints' g c)
type instance Constraints' (f :.: g) c = Constraints' g c
type instance Constraints' Par1 c = ()
type instance Constraints' (Rec1 f) c = ()
type instance Constraints' (K1 i a) c = c a
type instance Constraints' (M1 i t f) c = Constraints' f c

-- | `Constraints` is a constraint type synonym, containing the constraint
-- requirements for an instance for `t` of class `c`.
-- It requires an instance of class `c` for each component of `t`.
type Constraints t c = Constraints' (Rep t) c
