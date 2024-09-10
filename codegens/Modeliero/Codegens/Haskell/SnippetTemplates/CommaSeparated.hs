module Modeliero.Codegens.Haskell.SnippetTemplates.CommaSeparated where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

newtype CommaSeparated a = CommaSeparated
  { elements :: [a]
  }

instance (BroadPrinting a) => BroadPrinting (CommaSeparated a) where
  toBroadBuilder params =
    params.elements
      & fmap (toBroadBuilder)
      & intercalate ",\n"
      & indent 2
