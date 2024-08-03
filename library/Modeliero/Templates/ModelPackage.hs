{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Templates.ModelPackage where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Namespace qualified as Namespace
import Modeliero.Dsls.Package qualified as Package
import Modeliero.Templates.ModelPackage.Params qualified as Params

type Params = Params.Model

data Result = Result
  { package :: Package.Package
  }

compile :: Params -> Result
compile =
  error "TODO"
