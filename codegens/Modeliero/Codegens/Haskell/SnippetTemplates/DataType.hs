module Modeliero.Codegens.Haskell.SnippetTemplates.DataType where

import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate hiding (DataType)
import Modeliero.Codegens.Haskell.SnippetTemplates.DerivingVia

data DataType = DataType
  { name :: TextBlock,
    haddock :: Text,
    baseTypeSig :: TextBlock,
    stockDerivings :: [TextBlock],
    newtypeDerivings :: [TextBlock],
    -- | Deriving via text block.
    derivingVia :: Maybe DerivingVia
  }

instance BroadPrinting DataType where
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
