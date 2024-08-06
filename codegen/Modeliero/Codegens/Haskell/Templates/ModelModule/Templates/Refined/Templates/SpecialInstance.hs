module Modeliero.Codegens.Haskell.Templates.ModelModule.Templates.Refined.Templates.SpecialInstance where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { typeName :: Text,
    type_ :: Type,
    specialClassImport :: Import
  }

type Result = InModule TextBlock

data Type
  = IntType Integer Integer
  | TextType Int Int

compile :: Params -> Result
compile params = do
  textQfr <- requestImport Imports.text
  specialClassQfr <- requestImport params.specialClassImport
  (baseType :: TextBlock, specializeBody) <- case params.type_ of
    IntType min max ->
      pure
        ( "Int",
          [j|
            do
              when (value < $min) $$
                Left ("Value is smaller than $min: " <> fromString (show value))
              when (value > $max) $$
                Left ("Value is larger than $max: " <> fromString (show value))
              pure (${params.typeName} value)
          |]
        )
    TextType minLength maxLength -> do
      pure
        ( [j|
            ${textQfr}Text
          |],
          [j|
            do
              let length = ${textQfr}length value
              when (length < $minLength) $$
                Left ("Length is smaller than $minLength: " <> fromString (show length))
              when (length > $maxLength) $$
                Left ("Length is larger than $maxLength: " <> fromString (show length))
              pure (${params.typeName} value)
          |]
        )
  pure
    [j|
      instance ${specialClassQfr}Special ${params.typeName} where
        type GeneralizationOf ${params.typeName} = $baseType
        type SpecializationError ${params.typeName} = ${textQfr}Text
        specialize value = $specializeBody
        generalize (${params.typeName} base) = base
    |]
