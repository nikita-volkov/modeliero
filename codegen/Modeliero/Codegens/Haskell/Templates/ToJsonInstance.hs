module Modeliero.Codegens.Haskell.Templates.ToJsonInstance where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { typeName :: Text,
    structure :: Structure
  }

type Result = InModule TextBlock

data Structure
  = ProductStructure [Field]

data Field = Field
  { haskellName :: Text,
    jsonName :: Text
  }

compile :: Params -> Result
compile params = do
  aesonQfr <- import_ Imports.aeson
  aesonKeyMapQfr <- import_ Imports.aesonKeyMap
  let toJsonBody = case params.structure of
        ProductStructure fields ->
          [j|
            (${aesonQfr}Object . ${aesonKeyMapQfr}fromList)
              [ $fieldJsonExps
              ]
          |]
          where
            fieldJsonExps =
              fields
                & fmap
                  ( \field ->
                      [j|
                        ("${field.jsonName}", ${aesonQfr}toJSON value.${field.haskellName})
                      |]
                  )
                & TextBlock.intercalate ",\n"
                & TextBlock.indent 2

  pure
    [j|
      instance ${aesonQfr}ToJSON ${params.typeName} where
        toJSON value = $toJsonBody
    |]
