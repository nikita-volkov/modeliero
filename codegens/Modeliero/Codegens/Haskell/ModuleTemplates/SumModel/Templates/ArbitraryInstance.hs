module Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.ArbitraryInstance
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder
import Coalmine.Prelude hiding (intercalate)
import Modeliero.Codegens.Haskell.Snippets qualified as Snippets

data Params = Params
  { name :: TextBlock,
    variants :: [Variant],
    quickCheckArbitraryQfr :: TextBlock,
    quickCheckGenQfr :: TextBlock
  }

data Variant = Variant
  { constructorName :: TextBlock,
    memberNames :: [TextBlock]
  }

compile :: Params -> TextBlock
compile params =
  [j|
    instance ${params.quickCheckArbitraryQfr}Arbitrary ${params.name} where
      arbitrary =
        ${params.quickCheckGenQfr}oneof
          [ ${arbitraryExps}
          ]
      shrink = \case
        ${shrinkMatches}
  |]
  where
    arbitraryExps =
      params.variants
        & fmap
          ( \variant ->
              variant.memberNames
                & fmap
                  ( \_ ->
                      mconcat
                        [ params.quickCheckArbitraryQfr,
                          "arbitrary"
                        ]
                  )
                & Snippets.multilineApOr
                  (mconcat ["pure ", variant.constructorName])
                  variant.constructorName
          )
        & intercalate ",\n"
        & indent 2

    shrinkMatches =
      params.variants
        & fmap
          ( \variant ->
              mconcat
                [ variant.constructorName,
                  foldMap (" " <>) variant.memberNames,
                  " ->",
                  (indent 2)
                    ( mconcat
                        [ "\n",
                          variant.memberNames
                            & fmap
                              ( \memberName ->
                                  mconcat
                                    [ params.quickCheckArbitraryQfr,
                                      "shrink ",
                                      memberName
                                    ]
                              )
                            & Snippets.multilineApOr
                              "[]"
                              variant.constructorName
                        ]
                    )
                ]
          )
        & intercalate "\n"
