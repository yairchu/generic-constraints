-- | Write short and concise contexts based on generics.
--
-- Instead of writing boiler-plate standalone deriving clauses in the form of
--
-- > deriving instance [Various Eq Constraints Here] => Instance Eq MyType
--
-- You can use
--
-- > deriving instance Constraints MyType Eq => Eq MyType
--
-- And with the `TemplateHaskell` helpers @makeDeriving@ and others,
-- your declarations can be even more concise.

{-# LANGUAGE
    PolyKinds
  , TypeFamilies
  , TypeOperators
  , ConstraintKinds
  , TemplateHaskell
  #-}
module Generics.Constraints
    ( Constraints
      -- | Template Haskell helpers
    , makeDeriving, makeDerivings
    , makeInstance, makeInstances
    ) where

import           Data.Kind (Constraint, Type)
import           GHC.Generics
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Datatype as D

type family Constraints' (t :: Type -> Type) (c :: Type -> Constraint) :: Constraint
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
--
-- Useful for the writing the context of `StandaloneDeriving` clauses:
--
-- > deriving instance Constraints MyType Eq => Eq MyType
type Constraints t c = Constraints' (Rep t) c

-- | `TemplateHaskell` generation of multiple standalone deriving declarations.
--
-- > makeDerivings [''Eq, ''Ord, ''Show] [''MyType, ''MyOtherType]
--
-- Generates the deriving declarations for all given classes and types.
makeDerivings :: [T.Name] -> [T.Name] -> T.DecsQ
makeDerivings = makeMany makeDeriving

-- | `TemplateHaskell` generation of multiple instance declarations.
--
-- > makeInstances [''Binary, ''NFData] [''MyType, ''MyOtherType]
--
-- Generates the instances declarations for all given classes and types.
makeInstances :: [T.Name] -> [T.Name] -> T.DecsQ
makeInstances = makeMany makeInstance

makeMany :: (T.Name -> T.Name -> T.DecsQ) -> [T.Name] -> [T.Name] -> T.DecsQ
makeMany f classes types = concat <$> sequence (f <$> classes <*> types)

-- | `TemplateHaskell` generation of a standalone deriving declaration.
--
-- > makeDeriving ''Ord ''MyType
--
-- Generates:
--
-- > deriving instance Constraints MyType Ord => Ord MyType
makeDeriving :: T.Name -> T.Name -> T.DecsQ
makeDeriving = makeCommon T.standaloneDerivD

-- | `TemplateHaskell` generation of an instance declaration with no methods.
--
-- > makeDeriving ''Binary ''MyType
--
-- Generates:
--
-- > instance Constraints MyType Binary => Binary MyType
makeInstance :: T.Name -> T.Name -> T.DecsQ
makeInstance = makeCommon (\c i -> T.instanceD c i [])

makeCommon :: (T.CxtQ  -> T.TypeQ -> T.DecQ) -> T.Name -> T.Name -> T.DecsQ
makeCommon f clsName typName =
    do
        info <- D.reifyDatatype typName
        let typ = foldl T.appT (T.conT typName) (T.varT . D.tvName <$> D.datatypeVars info)
        pure <$> f (pure <$> [t|Constraints $typ $c|]) [t|$c $typ|]
    where
        c = T.conT clsName
