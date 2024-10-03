module Modeliero.Codegens.Haskell.SnippetTemplates.AdtHashableInstanceConstructor where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data AdtHashableInstanceConstructor = AdtHashableInstanceConstructor
  { name :: TextBlock,
    memberNames :: [TextBlock],
    hashableQfr :: TextBlock,
    saltExp :: TextBlock,
    index :: Int
  }

instance BroadPrinting AdtHashableInstanceConstructor where
  toBroadBuilder params =
    [j|
      ${params.name}${memberPatterns} ->
        ${exp}
    |]
    where
      memberPatterns =
        foldMap (mappend " ") params.memberNames
      exp =
        mconcat
          [ params.saltExp,
            mconcat
              [ "(",
                toBroadBuilder @Int params.index,
                " :: Int)"
              ]
              & extend,
            params.memberNames
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
