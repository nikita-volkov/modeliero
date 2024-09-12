module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.DataTypeDeclarationField where

import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data DataTypeDeclarationField = DataTypeDeclarationField
  { name :: TextBlock,
    type_ :: TextBlock,
    haddock :: Text
  }

instance BroadPrinting DataTypeDeclarationField where
  toBroadBuilder DataTypeDeclarationField {..} =
    mconcat
      [ filtered
          (not . Text.null)
          (flip mappend "\n" . prefixHaddock)
          haddock,
        name,
        " :: ",
        type_
      ]
