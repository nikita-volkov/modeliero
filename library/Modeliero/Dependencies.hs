module Modeliero.Dependencies where

import Modeliero.Dsls.Package qualified as Package

quickCheck :: Package.Dependency
quickCheck =
  Package.Dependency
    { name = "QuickCheck",
      minVersion = Package.Version 2 [15],
      maxVersion = Package.Version 3 []
    }
