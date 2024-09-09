module Modeliero.Codegens.Haskell.SnippetTemplates.RefinedTextLiteralInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data RefinedTextLiteralInstance = RefinedTextLiteralInstance
  { name :: TextBlock,
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    textQfr :: Text,
    literalQfr :: Text,
    basePreludeQfr :: Text,
    attoparsecQfr :: Text
  }

instance BroadPrinting RefinedTextLiteralInstance where
  toBroadBuilder params =
    [j|
      instance ${params.literalQfr}Literal ${params.name} where
        literalParser = do
          text <- ${params.attoparsecQfr}takeText
          ${minLengthCheckLines}${maxLengthCheckLines}pure (${params.name} text)
        literalToText (${params.name} text) = text
    |]
    where
      minLengthCheckLines =
        case params.minLength of
          Nothing -> ""
          Just minLength ->
            [j|
              when (${params.textQfr}length text < ${minLength}) do
                ${params.basePreludeQfr}fail "Length is smaller than ${minLength}"

            |]
      maxLengthCheckLines =
        case params.maxLength of
          Nothing -> ""
          Just maxLength ->
            [j|
              when (${params.textQfr}length text > ${maxLength}) do
                ${params.basePreludeQfr}fail "Length is smaller than ${maxLength}"

            |]
