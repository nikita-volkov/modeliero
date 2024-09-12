module Modeliero.Codegens.Haskell.SnippetTemplates.NewtypeDeclaration where

import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.DerivingVia

data NewtypeDeclaration = NewtypeDeclaration
  { name :: TextBlock,
    haddock :: Text,
    baseTypeSig :: TextBlock,
    stockDerivings :: [TextBlock],
    newtypeDerivings :: [TextBlock],
    -- | Deriving via text block.
    derivingVia :: Maybe DerivingVia
  }

instance BroadPrinting NewtypeDeclaration where
  toBroadBuilder params =
    [ filtered (not . Text.null) (flip mappend "\n" . Snippets.prefixHaddock) params.haddock,
      [j|
        newtype ${params.name} = ${params.name}
          { base :: ${params.baseTypeSig}
          }
      |],
      [ case params.stockDerivings of
          [] -> mempty
          _ -> "\nderiving stock (" <> intercalate ", " params.stockDerivings <> ")",
        case params.newtypeDerivings of
          [] -> mempty
          _ -> "\nderiving newtype (" <> intercalate ", " params.newtypeDerivings <> ")",
        params.derivingVia
          & foldMap (mappend "\n" . toBroadBuilder)
      ]
        & mconcat
        & indent 2
    ]
      & mconcat
