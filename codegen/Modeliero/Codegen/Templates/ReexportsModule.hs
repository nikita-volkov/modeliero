module Modeliero.Codegen.Templates.ReexportsModule where

import Coalmine.Prelude
import Modeliero.Codegen.Dsls.Code qualified as Code
import Modeliero.Codegen.Dsls.Namespace qualified as Namespace
import Modeliero.Codegen.Dsls.Package qualified as Package

data Params = Params
  { namespace :: Namespace.Namespace,
    modules :: [Namespace.Namespace]
  }

data Result = Result
  { module_ :: Package.Module,
    code :: Code.Code
  }

compile :: Params -> Result
compile =
  error "TODO"
