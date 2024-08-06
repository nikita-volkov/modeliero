module Modeliero.Codegens.Haskell.Templates.ModelModule.Templates.Product.Templates.FromJsonInstance where

import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as CodegenKitLegacy.Snippets
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { typeName :: Text,
    fields :: [Field]
  }

type Result = InModule TextBlock

data Field = Field
  { haskellName :: Text,
    jsonName :: Text,
    nullable :: Bool
  }

compile :: Params -> Result
compile params = do
  aesonQfr <- requestImport Imports.aeson
  aesonTypesQfr <- requestImport Imports.aesonTypes
  let fieldStatements =
        params.fields
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
        parseJSON = \case
          ${aesonQfr}Object object -> do
            ${fieldStatements}pure ${params.typeName}{..}
          json -> ${aesonTypesQfr}typeMismatch "Object" json
    |]
