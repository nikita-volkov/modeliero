module Modeliero.Codegen.Templates.ReexportsModule where

import Coalmine.Prelude
import Modeliero.Codegen.Dsls.InModule

data Params = Params
  { models :: [Text]
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  for_ params.models \reexportName -> do
    reexport Import {dependency = Nothing, name = reexportName}

  pure mempty
