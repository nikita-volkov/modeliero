module Modeliero.Codegens.Haskell.SnippetTemplates.ForcedProxyAnonymizableInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data ForcedProxyAnonymizableInstance = ForcedProxyAnonymizableInstance
  { name :: TextBlock,
    modelieroBaseQfr :: Text
  }

instance BroadPrinting ForcedProxyAnonymizableInstance where
  toBroadBuilder params =
    [j|
      instance ${params.modelieroBaseQfr}Anonymizable ${params.name} where
        anonymize _ (${params.name} base) =
          ${params.modelieroBaseQfr}anonymize True base
            & ${params.name}
    |]
