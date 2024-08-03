{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Templates.ProductArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Namespace qualified as Namespace
import Modeliero.Dsls.Package qualified as Package

data Params = Params
  {
  }

data Result = Result
  { package :: Package.Package
  }

compile :: Params -> Result
compile =
  error "TODO"
