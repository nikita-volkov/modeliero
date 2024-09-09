module Modeliero.Codegens.Haskell.ModuleTemplates.ProxyModel.Templates.IsomorpicToInstances where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: Text,
    baseType :: Text
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  isomorphismClassQfr <- requestImport Imports.isomorphismClass
  coerceQfr <- requestImport Imports.dataCoerce
  pure
    [j|
      instance ${isomorphismClassQfr}IsomorphicTo ${params.baseType} ${params.name} where
        to = ${coerceQfr}coerce
      
      instance ${isomorphismClassQfr}IsomorphicTo ${params.name} ${params.baseType} where
        to = ${coerceQfr}coerce
    |]
