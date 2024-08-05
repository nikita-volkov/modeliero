{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegen.Templates.ModelPackage where

import Coalmine.Prelude
import Modeliero.Codegen.Dsls.Code qualified as Code
import Modeliero.Codegen.Dsls.Namespace qualified as Namespace
import Modeliero.Codegen.Dsls.Package qualified as Package
import Modeliero.Codegen.Templates.ModelPackage.Params qualified as Params

type Params = Params.Model

data Result = Result
  { package :: Package.Package
  }

compile :: Params -> Result
compile =
  error "TODO"
