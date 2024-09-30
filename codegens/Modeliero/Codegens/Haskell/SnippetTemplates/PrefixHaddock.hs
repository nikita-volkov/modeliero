module Modeliero.Codegens.Haskell.SnippetTemplates.PrefixHaddock where

import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

newtype PrefixHaddock = PrefixHaddock
  { text :: Text
  }

instance BroadPrinting PrefixHaddock where
  toBroadBuilder params =
    filtered (not . Text.null) Snippets.prefixHaddock params.text
