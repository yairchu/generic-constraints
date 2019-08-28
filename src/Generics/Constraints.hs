{-# LANGUAGE
    PolyKinds
  , TypeFamilies
  , TypeOperators
  , ConstraintKinds
  , TemplateHaskellQuotes
  #-}
module Generics.Constraints
    ( Constraints
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
type Constraints t c = Constraints' (Rep t) c

makeDerivings :: [T.Name] -> [T.Name] -> T.DecsQ
makeDerivings = makeMany makeDeriving

makeInstances :: [T.Name] -> [T.Name] -> T.DecsQ
makeInstances = makeMany makeDeriving

makeMany :: (T.Name -> T.Name -> T.DecsQ) -> [T.Name] -> [T.Name] -> T.DecsQ
makeMany f classes types = concat <$> sequence (f <$> classes <*> types)

makeDeriving :: T.Name -> T.Name -> T.DecsQ
makeDeriving = makeCommon (T.StandaloneDerivD Nothing)

makeInstance :: T.Name -> T.Name -> T.DecsQ
makeInstance = makeCommon (\c i -> T.InstanceD Nothing c i [])

makeCommon :: ([T.Type] -> T.Type -> T.Dec) -> T.Name -> T.Name -> T.DecsQ
makeCommon f clsName typName =
    r <$> D.reifyDatatype typName
    where
        r info =
            [ f [T.ConT ''Constraints `T.AppT` typ `T.AppT` T.ConT clsName]
                (T.ConT clsName `T.AppT` typ)
            ]
            where
                typ =
                    foldl T.AppT (T.ConT typName)
                    (T.VarT . D.tvName <$> D.datatypeVars info)
