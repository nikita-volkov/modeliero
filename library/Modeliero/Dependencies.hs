module Modeliero.Dependencies where

import Coalmine.Prelude
import Modeliero.Dsls.Package qualified as Package

quickCheck :: Package.Dependency
quickCheck =
  Package.Dependency
    { name = "QuickCheck",
      minVersion = $$(l "2.15"),
      maxVersion = $$(l "3")
    }
