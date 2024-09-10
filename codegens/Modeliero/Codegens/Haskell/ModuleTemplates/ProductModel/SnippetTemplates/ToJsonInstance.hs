module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.ToJsonInstance where

import Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.ToJsonInstanceFieldExp
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.CommaSeparated

data ToJsonInstance = ToJsonInstance
  { typeName :: TextBlock,
    aesonQfr :: TextBlock,
    aesonKeyMapQfr :: TextBlock,
    fields :: CommaSeparated ToJsonInstanceFieldExp
  }

instance BroadPrinting ToJsonInstance where
  toBroadBuilder ToJsonInstance {..} =
    [j|
      instance ${aesonQfr}ToJSON ${typeName} where
        toJSON value =
          (${aesonQfr}Object . ${aesonKeyMapQfr}fromList . catMaybes)
            [ ${fields}
            ]
    |]
