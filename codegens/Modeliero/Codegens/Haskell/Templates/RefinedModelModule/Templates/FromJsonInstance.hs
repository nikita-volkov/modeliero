module Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.FromJsonInstance where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: Text,
    type_ :: Type
  }

type Result = InModule TextBlock

data Type
  = IntType

compile :: Params -> Result
compile params = do
  aesonQfr <- requestImport Imports.aeson
  aesonTypesQfr <- requestImport Imports.aesonTypes
  body <- case params.type_ of
    IntType -> do
      scientificQfr <- requestImport Imports.scientific
      specialQfr <- requestImport Imports.special
      textQfr <- requestImport Imports.text
      pure
        [j|
          do
            scientific <-
              case json of
                ${aesonQfr}Number scientific -> pure scientific
                _ -> ${aesonTypesQfr}typeMismatch "Number" json
            int <-
              if ${scientificQfr}isInteger scientific
                then case ${scientificQfr}toBoundedInteger scientific of
                  Just int -> pure int
                  Nothing -> fail ("Number " <> show scientific <> " is out of Int bounds")
                else fail ("Number " <> show scientific <> " is not an integer")
            case ${specialQfr}specialize int of
              Right result -> pure result
              Left text -> fail (${textQfr}unpack text)
        |]

  pure
    [j|
      instance ${aesonQfr}FromJSON ${params.name} where
        parseJSON json = $body
    |]
