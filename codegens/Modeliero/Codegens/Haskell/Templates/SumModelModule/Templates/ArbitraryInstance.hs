module Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.ArbitraryInstance
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder
import Coalmine.Prelude hiding (intercalate)
import Modeliero.Codegens.Haskell.Snippets qualified as MoreSnippets

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
          [ $arbitraryExps
          ]
      shrink = \case
        $shrinkMatches
      
  |]
  where
    arbitraryExps =
      params.variants
        & fmap
          ( \variant ->
              mconcat
                [ variant.constructorName,
                  " <$> ",
                  params.quickCheckArbitraryQfr,
                  "arbitrary"
                ]
          )
        & intercalate ",\n  "

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
                            & nonEmpty
                            & \case
                              Just members ->
                                MoreSnippets.multilineAp
                                  variant.constructorName
                                  members
                              Nothing ->
                                "[]"
                        ]
                    )
                ]
          )
        & intercalate "\n"
