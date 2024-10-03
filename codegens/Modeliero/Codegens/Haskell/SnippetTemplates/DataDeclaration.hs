module Modeliero.Codegens.Haskell.SnippetTemplates.DataDeclaration where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.DataDeclarationVariant
import Modeliero.Codegens.Haskell.SnippetTemplates.DerivingVia
import Modeliero.Codegens.Haskell.SnippetTemplates.PrefixHaddock

data DataDeclaration = DataDeclaration
  { name :: TextBlock,
    haddock :: Maybe PrefixHaddock,
    variants :: [DataDeclarationVariant],
    stockDerivings :: [TextBlock],
    derivingVia :: [DerivingVia]
  }

instance BroadPrinting DataDeclaration where
  toBroadBuilder params =
    mconcat
      [ params.haddock
          & fmap toBroadBuilder
          & fmap (flip mappend "\n")
          & fold,
        "data ",
        params.name,
        params.variants
          & fmap toBroadBuilder
          & \case
            [] -> mempty
            h : t ->
              [ "\n= ",
                indent 2 h,
                t
                  & fmap (indent 2)
                  & fmap (mappend "\n| ")
                  & fold
              ]
                & mconcat
                & indent 2,
        case params.stockDerivings of
          [] -> mempty
          derivingSplices -> "\n  deriving stock (" <> TextBlock.intercalate ", " derivingSplices <> ")",
        params.derivingVia
          & foldMap (indent 2 . mappend "\n" . toBroadBuilder)
      ]
