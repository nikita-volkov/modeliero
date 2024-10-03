module Modeliero.Codegens.Haskell.SnippetTemplates.EnumArbitraryInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data EnumArbitraryInstance = EnumArbitraryInstance
  { name :: TextBlock,
    basePreludeQfr :: TextBlock,
    quickCheckArbitraryQfr :: TextBlock,
    quickCheckGenQfr :: TextBlock
  }

instance BroadPrinting EnumArbitraryInstance where
  toBroadBuilder params =
    [j|
      instance ${params.quickCheckArbitraryQfr}Arbitrary ${params.name} where
        arbitrary =
          ${params.quickCheckGenQfr}chooseEnum (${params.basePreludeQfr}minBound, ${params.basePreludeQfr}maxBound)
    |]
