module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.HashableInstance where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data HashableInstance = HashableInstance
  { typeName :: TextBlock,
    hashableQfr :: TextBlock,
    fieldAccessors :: [TextBlock]
  }

instance BroadPrinting HashableInstance where
  toBroadBuilder HashableInstance {..} =
    [j|
      instance ${hashableQfr}Hashable ${typeName} where
        hashWithSalt salt value =
          salt${hashWithSaltExp}
    |]
    where
      hashWithSaltExp =
        fieldAccessors
          & fmap
            ( \accessor ->
                [j|
                  
                  & flip ${hashableQfr}hashWithSalt value.${accessor}
                |]
            )
          & fmap (indent 2)
          & mconcat
