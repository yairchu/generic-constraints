{-# LANGUAGE
    PolyKinds
  , TypeFamilies
  , TypeOperators
  , ConstraintKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}
module Generics.Constraints.Internal (Constraints) where

import GHC.Generics
import GHC.Types (Constraint)

type family Constraints' (t :: * -> *) (t' :: * -> *) (c :: * -> * -> Constraint) (c1 :: (* -> *) -> (* -> *) -> Constraint) :: Constraint
type instance Constraints' V1 V1 c c1 = ()
type instance Constraints' U1 U1 c c1 = ()
type instance Constraints' (f :+: g) (f' :+: g') c c1 = (Constraints' f f' c c1, Constraints' g g' c c1)
type instance Constraints' (f :*: g) (f' :*: g') c c1 = (Constraints' f f' c c1, Constraints' g g' c c1)
type instance Constraints' (f :.: g) (f' :.: g') c c1 = (c1 f f', Constraints' g g' c c1)
type instance Constraints' Par1 Par1 c c1 = ()
type instance Constraints' (Rec1 f) (Rec1 g) c c1 = c1 f g
type instance Constraints' (K1 i a) (K1 i' b) c c1 = c a b
type instance Constraints' (M1 i t f) (M1 i' t' f') c c1 = Constraints' f f' c c1

-- | `Constraints` is a constraint type synonym, containing the constraint
-- requirements for an instance for `t` of class `c`.
-- It requires an instance of class `c` for each component of `t`.
type Constraints t t' c = Constraints' (Rep t) (Rep t') c AnyType

class AnyType a b
instance AnyType a b
