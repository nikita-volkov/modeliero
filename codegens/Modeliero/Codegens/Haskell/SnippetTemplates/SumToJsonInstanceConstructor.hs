module Modeliero.Codegens.Haskell.SnippetTemplates.SumToJsonInstanceConstructor where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.StringLiteral

data SumToJsonInstanceConstructor = SumToJsonInstanceConstructor
  { constructorName :: TextBlock,
    jsonName :: StringLiteral,
    memberNames :: [TextBlock],
    aesonKeyMapQfr :: TextBlock,
    aesonQfr :: TextBlock
  }

instance BroadPrinting SumToJsonInstanceConstructor where
  toBroadBuilder params =
    [j|
      ${params.constructorName}${memberPatterns} ->
        ${jsonExp}
    |]
    where
      memberPatterns =
        foldMap (mappend " ") params.memberNames

      jsonExp =
        case params.memberNames of
          [] ->
            [j|${params.aesonQfr}String ${params.jsonName}|]
          [memberName1] ->
            [j|
              ${memberName1}
                & ${params.aesonQfr}toJSON
                & ${params.aesonKeyMapQfr}singleton ${params.jsonName}
                & ${params.aesonQfr}toJSON
            |]
          memberNames ->
            [j|
              ${valueExp}
                & ${params.aesonQfr}toJSON
                & ${params.aesonKeyMapQfr}singleton ${params.jsonName}
                & ${params.aesonQfr}toJSON
            |]
            where
              valueExp =
                multilineList memberNames
