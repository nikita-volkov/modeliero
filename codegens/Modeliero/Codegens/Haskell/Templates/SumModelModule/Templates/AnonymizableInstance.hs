module Modeliero.Codegens.Haskell.Templates.SumModelModule.Templates.AnonymizableInstance
  ( Params (..),
    Variant (..),
    VariantField (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder
import Coalmine.Prelude hiding (intercalate)

data Params = Params
  { name :: TextBlock,
    variants :: [Variant],
    anonymizableQfr :: TextBlock
  }

data Variant = Variant
  { constructorName :: TextBlock,
    fields :: [VariantField]
  }

data VariantField = VariantField
  { name :: TextBlock,
    anonymizable :: Bool
  }

compile :: Params -> TextBlock
compile params =
  [j|
    instance ${params.anonymizableQfr}Anonymizable ${params.name} where
      anonymize forced = \case
        ${matches}
  |]
  where
    matches =
      params.variants
        & fmap
          ( \variant ->
              mconcat
                [ variant.constructorName,
                  variant.fields
                    & fmap (.name)
                    & foldMap (mappend " "),
                  " ->",
                  (indent 2)
                    ( mconcat
                        [ "\n",
                          variant.constructorName,
                          (indent 2)
                            ( variant.fields
                                & foldMap
                                  ( \field ->
                                      mconcat
                                        [ "\n(",
                                          params.anonymizableQfr,
                                          "anonymize ",
                                          if field.anonymizable
                                            then "True"
                                            else "forced",
                                          " ",
                                          field.name,
                                          ")"
                                        ]
                                  )
                            )
                        ]
                    )
                ]
          )
        & intercalate "\n"
