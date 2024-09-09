module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.Templates.ToJsonInstance where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { typeName :: Text,
    fields :: [Field]
  }

type Result = InModule TextBlock

data Field = Field
  { haskellName :: Text,
    jsonName :: Text
  }

compile :: Params -> Result
compile params = do
  aesonQfr <- requestImport Imports.aeson
  aesonKeyMapQfr <- requestImport Imports.aesonKeyMap
  let fieldJsonExps =
        params.fields
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
        toJSON value =
          (${aesonQfr}Object . ${aesonKeyMapQfr}fromList)
            [ ${fieldJsonExps}
            ]
    |]
