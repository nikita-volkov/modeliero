module Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.ToJsonInstance
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets

data Params = Params
  { name :: TextBlock,
    variants :: [Variant],
    aesonQfr :: TextBlock,
    aesonKeyMapQfr :: TextBlock
  }

data Variant = Variant
  { constructorName :: TextBlock,
    varName :: TextBlock,
    jsonName :: Text,
    memberNames :: [TextBlock]
  }

compile :: Params -> TextBlock
compile params =
  [i|
    instance ${params.aesonQfr}ToJSON ${params.name} where
      toJSON = \case
        ${matches}
  |]
  where
    matches =
      params.variants
        & fmap compileVariant
        & TextBlock.intercalate "\n"
      where
        compileVariant variant =
          [j|
            ${variant.constructorName}${params.name}${memberPatterns} ->
              ${variant.varName}
                & ${params.aesonQfr}toJSON
                & ${params.aesonKeyMapQfr}singleton ${keyExp}
                & ${params.aesonQfr}toJSON
          |]
          where
            memberPatterns =
              foldMap (mappend " ") variant.memberNames
            keyExp =
              Snippets.stringLiteral variant.jsonName
