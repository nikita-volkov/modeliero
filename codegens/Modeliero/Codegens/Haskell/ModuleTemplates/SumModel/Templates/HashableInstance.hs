module Modeliero.Codegens.Haskell.ModuleTemplates.SumModel.Templates.HashableInstance
  ( Params (..),
    Variant (..),
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude

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
  [i|
    instance ${params.hashableQfr}Hashable ${params.name} where
      hashWithSalt salt = \case
        ${matches}
  |]
  where
    matches =
      params.variants
        & zip (enumFrom 0)
        & fmap (uncurry compileVariant)
        & TextBlock.intercalate "\n"
      where
        compileVariant index variant =
          [j|
            ${variant.name}${params.name}${memberPatterns} ->
              ${exp}
          |]
          where
            memberPatterns =
              foldMap (mappend " ") variant.memberNames
            exp =
              mconcat
                [ "salt",
                  mconcat
                    [ "(",
                      toBroadBuilder @Int index,
                      " :: Int)"
                    ]
                    & extend,
                  variant.memberNames
                    & foldMap extend
                ]
              where
                extend hashedExp =
                  mconcat
                    [ "\n  & flip ",
                      params.hashableQfr,
                      "hashWithSalt ",
                      hashedExp
                    ]
