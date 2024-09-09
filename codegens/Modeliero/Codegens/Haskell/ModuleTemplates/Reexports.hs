module Modeliero.Codegens.Haskell.ModuleTemplates.Reexports where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule

data Params = Params
  { models :: [Text]
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  for_ params.models \reexportName -> do
    registerReexport Import {dependency = Nothing, name = reexportName}

  pure mempty
