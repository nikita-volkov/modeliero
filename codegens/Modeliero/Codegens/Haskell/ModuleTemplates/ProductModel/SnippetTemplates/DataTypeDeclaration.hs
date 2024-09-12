module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.DataTypeDeclaration where

import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.DataTypeDeclarationField
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data DataTypeDeclaration = DataTypeDeclaration
  { name :: TextBlock,
    haddock :: Text,
    fields :: [DataTypeDeclarationField],
    stockDerivings :: [TextBlock]
  }

instance BroadPrinting DataTypeDeclaration where
  toBroadBuilder DataTypeDeclaration {..} =
    mconcat
      [ filtered (not . Text.null) (flip mappend "\n" . prefixHaddock) haddock,
        dataRecordDecl
          name
          name
          (fmap toBroadBuilder fields),
        case stockDerivings of
          [] -> mempty
          _ -> "\n  deriving stock (" <> intercalate ", " stockDerivings <> ")"
      ]
