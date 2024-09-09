module Modeliero.Codegens.Haskell.SnippetTemplates.ProductAnonymizableInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data ProductAnonymizableInstance = ProductAnonymizableInstance
  { name :: TextBlock,
    fieldNames :: [TextBlock],
    anonymizable :: Bool,
    modelieroBaseQfr :: Text
  }

instance BroadPrinting ProductAnonymizableInstance where
  toBroadBuilder params =
    [j|
      instance ${params.modelieroBaseQfr}Anonymizable ${params.name} where
        anonymize forced product =
          ${params.name}
            { ${fieldsSplice}
            }
    |]
    where
      forcedSplice :: Text
      forcedSplice =
        if params.anonymizable
          then "True"
          else "forced"
      fieldsSplice =
        params.fieldNames
          & fmap (\name -> [j|${name} = ${params.modelieroBaseQfr}anonymize ${forcedSplice} product.${name}|])
          & intercalate ",\n"
          & indent 2
