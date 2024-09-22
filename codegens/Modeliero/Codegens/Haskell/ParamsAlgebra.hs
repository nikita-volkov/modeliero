module Modeliero.Codegens.Haskell.ParamsAlgebra
  ( module Modeliero.Codegens.Haskell.Params,
    module Modeliero.Codegens.Haskell.ParamsAlgebra,
  )
where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Params

-- | Leafs of the value type.
-- Useful for analysis.
valueTypePlainTypes :: ValueType -> [PlainType]
valueTypePlainTypes = \case
  PlainValueType plainType -> pure plainType
  MaybeValueType base -> valueTypePlainTypes base
  VectorValueType base -> valueTypePlainTypes base
  HashMapValueType key value -> valueTypePlainTypes key <> valueTypePlainTypes value

valueTypeLocalTypes :: ValueType -> [Slug]
valueTypeLocalTypes =
  valueTypePlainTypes >=> \case
    LocalPlainType ref -> pure ref
    _ -> empty

valueTypeCanBeKey :: (Slug -> Maybe ValueType) -> ValueType -> Bool
valueTypeCanBeKey derefLocal = \case
  PlainValueType plainType -> plainTypeCanBeKey derefLocal plainType
  MaybeValueType base -> valueTypeCanBeKey derefLocal base
  VectorValueType base -> valueTypeCanBeKeyAsVectorElement derefLocal base
  HashMapValueType _ _ -> False

valueTypeCanBeKeyAsVectorElement :: (Slug -> Maybe ValueType) -> ValueType -> Bool
valueTypeCanBeKeyAsVectorElement derefLocal = \case
  PlainValueType plainType -> plainTypeCanBeKey derefLocal plainType
  MaybeValueType _ -> False
  _ -> True

plainTypeCanBeKey :: (Slug -> Maybe ValueType) -> PlainType -> Bool
plainTypeCanBeKey derefLocal = \case
  LocalPlainType ref -> case derefLocal ref of
    Just valueType -> valueTypeCanBeKey derefLocal valueType
    Nothing -> False
  StandardPlainType _standardType -> True
  CustomPlainType _ -> True
