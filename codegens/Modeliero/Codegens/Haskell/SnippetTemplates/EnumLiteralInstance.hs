module Modeliero.Codegens.Haskell.SnippetTemplates.EnumLiteralInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data EnumLiteralInstance = EnumLiteralInstance
  { name :: TextBlock,
    variants :: [(TextBlock, Text)],
    literalQfr :: TextBlock,
    attoparsecQfr :: TextBlock
  }

instance BroadPrinting EnumLiteralInstance where
  toBroadBuilder params =
    [j|
      instance ${params.literalQfr}Literal ${params.name} where
        literalParser =
          asum
            [ ${parserAlternatives}
            ]
        literalToText = \case
          ${patterns}
    |]
    where
      (parserAlternatives, patterns) =
        params.variants
          & fmap
            ( \(constructor, textValue) ->
                let literalExp = stringLiteral textValue
                 in ( [j|${constructor} <$ ${params.attoparsecQfr}string ${literalExp}|],
                      [j|${constructor} -> ${literalExp}|]
                    )
            )
          & unzip
          & first (indent 2 . intercalate ",\n")
          & second (intercalate "\n")
