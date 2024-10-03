module Modeliero.Codegens.Haskell.SnippetTemplates.AdtHashableInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.AdtHashableInstanceConstructor

-- | General Hashable instance template, applicable to all ADT variations: products, sums, enums.
data AdtHashableInstance = AdtHashableInstance
  { name :: TextBlock,
    variants :: [AdtHashableInstanceConstructor],
    hashableQfr :: TextBlock,
    saltPattern :: TextBlock
  }

instance BroadPrinting AdtHashableInstance where
  toBroadBuilder params =
    [i|
      instance ${params.hashableQfr}Hashable ${params.name} where
        hashWithSalt ${params.saltPattern} = \case
          ${matches}
    |]
    where
      matches =
        params.variants
          & fmap toBroadBuilder
          & intercalate "\n"
