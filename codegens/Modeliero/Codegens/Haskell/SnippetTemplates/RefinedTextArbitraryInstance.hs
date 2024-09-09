module Modeliero.Codegens.Haskell.SnippetTemplates.RefinedTextArbitraryInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data RefinedTextArbitraryInstance = RefinedTextArbitraryInstance
  { name :: TextBlock,
    minLength :: Int,
    maxLength :: Int,
    quickCheckQfr :: Text
  }

instance BroadPrinting RefinedTextArbitraryInstance where
  toBroadBuilder params =
    [j|
      instance ${params.quickCheckQfr}Arbitrary ${params.name} where
        arbitrary = do
          size <- ${params.quickCheckQfr}chooseInt (${params.minLength}, ${params.maxLength})
          chars <- ${params.quickCheckQfr}vectorOf size ${params.quickCheckQfr}arbitrary
          pure (${params.name} (fromString chars))
    |]
