module Modeliero.Codegens.Haskell.SnippetTemplates.StringLiteral where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

newtype StringLiteral = StringLiteral
  { content :: Text
  }

instance BroadPrinting StringLiteral where
  toBroadBuilder params =
    stringLiteral params.content
