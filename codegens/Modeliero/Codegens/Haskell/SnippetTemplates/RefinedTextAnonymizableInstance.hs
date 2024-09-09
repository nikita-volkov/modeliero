module Modeliero.Codegens.Haskell.SnippetTemplates.RefinedTextAnonymizableInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data RefinedTextAnonymizableInstance = RefinedTextAnonymizableInstance
  { name :: TextBlock,
    minLength :: Int,
    maxLength :: Int,
    anonymizable :: Bool,
    modelieroBaseQfr :: Text,
    textQfr :: Text
  }

instance BroadPrinting RefinedTextAnonymizableInstance where
  toBroadBuilder params =
    [j|
      instance ${params.modelieroBaseQfr}Anonymizable ${params.name} where
        anonymize forced (${params.name} initial) = 
          ${params.name} adaptedToMaxLength
          where
            anonymized =
              ${params.modelieroBaseQfr}anonymize ${forcedSplice} initial
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
    where
      forcedSplice :: Text
      forcedSplice =
        if params.anonymizable
          then "True"
          else "forced"
