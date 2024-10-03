module Modeliero.Codegens.Haskell.SnippetTemplates.SumToJsonInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.SumToJsonInstanceConstructor

data SumToJsonInstance = SumToJsonInstance
  { name :: TextBlock,
    constructors :: [SumToJsonInstanceConstructor],
    aesonQfr :: TextBlock,
    aesonKeyMapQfr :: TextBlock
  }

instance BroadPrinting SumToJsonInstance where
  toBroadBuilder params =
    [i|
      instance ${params.aesonQfr}ToJSON ${params.name} where
        toJSON = \case
          ${matches}
    |]
    where
      matches =
        params.constructors
          & fmap toBroadBuilder
          & intercalate "\n"
