module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates.ToJsonInstanceFieldExp where

import Modeliero.Codegens.Haskell.Preludes.SnippetTemplate

data ToJsonInstanceFieldExp = ToJsonInstanceFieldExp
  { required :: Bool,
    key :: Text,
    valueExp :: TextBlock,
    aesonQfr :: TextBlock
  }

instance BroadPrinting ToJsonInstanceFieldExp where
  toBroadBuilder ToJsonInstanceFieldExp {..} =
    if required
      then
        [j|Just (${keyExp}, ${aesonQfr}toJSON ${valueExp})|]
      else
        [j|(${keyExp},) . ${aesonQfr}toJSON <$> ${valueExp}|]
    where
      keyExp = stringLiteral key
