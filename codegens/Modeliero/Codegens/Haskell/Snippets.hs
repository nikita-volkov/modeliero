module Modeliero.Codegens.Haskell.Snippets where

import Coalmine.MultilineTextBuilder
import Coalmine.Prelude hiding (intercalate)

multilineAp :: TextBlock -> NonEmpty TextBlock -> TextBlock
multilineAp mapExp (apExpsHead :| apExpsTail) =
  mapExp <> "\n  <$> " <> indent 4 apExpsHead <> foldMap (mappend "\n  <*> " . indent 4) apExpsTail

multilineApOr :: TextBlock -> TextBlock -> [TextBlock] -> TextBlock
multilineApOr emptyExp mapExp =
  maybe emptyExp (multilineAp mapExp) . nonEmpty
