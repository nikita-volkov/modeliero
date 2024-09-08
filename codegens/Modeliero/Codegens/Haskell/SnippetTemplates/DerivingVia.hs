module Modeliero.Codegens.Haskell.SnippetTemplates.DerivingVia where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate
import Modeliero.Codegens.Haskell.SnippetTemplates.ParensList

data DerivingVia = DerivingVia
  { derivings :: ParensList TextBlock,
    viaSig :: TextBlock
  }

instance BroadPrinting DerivingVia where
  toBroadBuilder params =
    [j|
      deriving
        ${params.derivings}
        via (${params.viaSig})
    |]
