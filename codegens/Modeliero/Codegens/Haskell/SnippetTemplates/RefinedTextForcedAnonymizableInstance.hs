module Modeliero.Codegens.Haskell.SnippetTemplates.RefinedTextForcedAnonymizableInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data RefinedTextForcedAnonymizableInstance = RefinedTextForcedAnonymizableInstance
  { name :: TextBlock,
    minLength :: Int,
    maxLength :: Int,
    modelieroBaseQfr :: Text,
    textQfr :: Text
  }

instance BroadPrinting RefinedTextForcedAnonymizableInstance where
  toBroadBuilder params =
    [j|
      instance ${params.modelieroBaseQfr}Anonymizable ${params.name} where
        anonymize _ (${params.name} initial) = 
          ${params.name} adaptedToMaxLength
          where
            anonymized =
              ${params.modelieroBaseQfr}anonymize True initial
            anonymizedLength =
              ${params.textQfr}length anonymized
            minLengthReplicationFactor =
              case divMod ${params.minLength} anonymizedLength of
                (d, m) -> if m > 0 then succ d else d
            adaptedToMinLength =
              anonymized
                & ${params.textQfr}replicate minLengthReplicationFactor
            adaptedToMaxLength =
              adaptedToMinLength
                & ${params.textQfr}take ${params.maxLength}
    |]
