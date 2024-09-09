module Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.ToJsonInstance where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: Text
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  aesonQfr <- requestImport Imports.aeson
  pure
    [j|
      instance ${aesonQfr}ToJSON ${params.name} where
        toJSON (${params.name} base) = ${aesonQfr}toJSON base
    |]
