module Modeliero.Codegens.Haskell.SnippetTemplates.ParensList where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

newtype ParensList a = ParensList
  { elements :: [a]
  }

instance (BroadPrinting a) => BroadPrinting (ParensList a) where
  toBroadBuilder params =
    [ "( ",
      params.elements
        & fmap (toBroadBuilder)
        & intercalate ",\n"
        & indent 2,
      "\n)"
    ]
      & mconcat
