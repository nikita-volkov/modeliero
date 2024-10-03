module Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.HashableInstance
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.SnippetTemplates qualified as SnippetTemplates

data Params = Params
  { name :: TextBlock,
    variants :: [Variant],
    hashableQfr :: TextBlock
  }

data Variant = Variant
  { name :: TextBlock,
    memberNames :: [TextBlock]
  }

compile :: Params -> TextBlock
compile params =
  SnippetTemplates.AdtHashableInstance
    { name = params.name,
      hashableQfr = params.hashableQfr,
      saltPattern = saltVarName,
      variants =
        params.variants
          & zip (enumFrom 0)
          & fmap
            ( \(index, variant) ->
                SnippetTemplates.AdtHashableInstanceConstructor
                  { name = variant.name,
                    memberNames = variant.memberNames,
                    hashableQfr = params.hashableQfr,
                    saltExp = saltVarName,
                    index
                  }
            )
    }
    & toBroadBuilder
  where
    saltVarName = "salt"
