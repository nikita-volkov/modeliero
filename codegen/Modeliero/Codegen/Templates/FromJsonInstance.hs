module Modeliero.Codegen.Templates.FromJsonInstance where

import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as CodegenKitLegacy.Snippets
import Modeliero.Codegen.Dsls.InModule
import Modeliero.Codegen.Imports qualified as Imports

data Params = Params
  { typeName :: Text,
    structure :: Structure
  }

type Result = InModule TextBlock

data Structure
  = ProductStructure [Field]

data Field = Field
  { haskellName :: Text,
    jsonName :: Text,
    nullable :: Bool
  }

compile :: Params -> Result
compile params = do
  aesonQfr <- import_ Imports.aeson
  aesonTypesQfr <- import_ Imports.aesonTypes
  let parseJsonBody = case params.structure of
        ProductStructure fields ->
          [j|
            \case
              ${aesonQfr}Object object -> do
                ${fieldStatements}pure ${params.typeName}{..}
              json -> ${aesonTypesQfr}typeMismatch "Object" json
          |]
          where
            fieldStatements =
              fields
                & foldMap
                  ( \field ->
                      let jsonNameLit = CodegenKitLegacy.Snippets.stringLiteral field.jsonName
                          parseExp :: Text =
                            if field.nullable
                              then "parseFieldMaybe"
                              else "parseField"
                       in [j|
                            ${field.haskellName} <- ${aesonTypesQfr}${parseExp} object ${jsonNameLit}

                          |]
                  )

  pure
    [j|
      instance ${aesonQfr}FromJSON ${params.typeName} where
        parseJSON = $parseJsonBody
    |]
