module Modeliero.Codegens.Haskell.SnippetTemplates.DataDeclarationVariant where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.PrefixHaddock

data DataDeclarationVariant = DataDeclarationVariant
  { name :: TextBlock,
    members :: [TextBlock],
    haddock :: Maybe PrefixHaddock
  }

instance BroadPrinting DataDeclarationVariant where
  toBroadBuilder variant =
    [ variant.haddock
        & fmap toBroadBuilder,
      (variant.name : variant.members)
        & intercalate " "
        & Just
    ]
      & catMaybes
      & intercalate "\n"
