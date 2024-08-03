module Modeliero.Templates.ReexportsModule where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Namespace qualified as Namespace
import Modeliero.Dsls.Package qualified as Package

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
