module Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.FromJsonInstance
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder
import Coalmine.Prelude hiding (intercalate)
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.List qualified as List

data Params = Params
  { name :: Text,
    variants :: [Variant],
    aesonQfr :: TextBlock,
    aesonKeyMapQfr :: TextBlock,
    aesonTypesQfr :: TextBlock
  }

data Variant = Variant
  { constructorName :: Text,
    varName :: Text,
    jsonName :: Text,
    memberNames :: [Text]
  }

compile :: Params -> TextBlock
compile params =
  [i|
    instance ${params.aesonQfr}FromJSON ${params.name} where
      parseJSON = \case
        ${params.aesonQfr}Object object ->
          [ ${variantExps}
          ]
            & asum
            & fromMaybe (${params.aesonTypesQfr}parseFail noTagFoundMessage)
          where
            noTagFoundMessage =
              "No expected key found. \
              \It should be one of the following: \
              \${keyList}"
        ${stringMatch}json -> ${params.aesonTypesQfr}typeMismatch "Object" json
  |]
  where
    keyList =
      params.variants
        & fmap
          ( \variant ->
              variant.jsonName
                & to
          )
        & intercalate ", "

    variantExps =
      params.variants
        & fmap
          ( \variant ->
              let nameLiteral = variant.jsonName & Snippets.stringLiteral
               in [j|
                    object
                      & ${params.aesonKeyMapQfr}lookup ${nameLiteral}
                      & fmap 
                        ( \json -> 
                          ${variant.constructorName}
                            <$> ${params.aesonQfr}parseJSON json
                            ${params.aesonTypesQfr}<?> ${params.aesonTypesQfr}Key ${nameLiteral}
                        )
                  |]
          )
        & intercalate ",\n"
        & indent 2

    stringMatch =
      case stringMatches of
        "" -> mempty
        _ ->
          [j|
            ${params.aesonQfr}String string -> case string of
                ${stringMatches}
          |]
      where
        stringMatches =
          params.variants
            & fmap
              ( \variant ->
                  if List.null variant.memberNames
                    then
                      mconcat
                        [ variant.jsonName
                            & Snippets.stringLiteral,
                          " -> ",
                          to variant.constructorName
                        ]
                        & Just
                    else Nothing
              )
            & catMaybes
            & intercalate "\n"
