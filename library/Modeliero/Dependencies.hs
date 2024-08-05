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

text :: Package.Dependency
text =
  Package.Dependency
    { name = "text",
      minVersion = $$(l "2"),
      maxVersion = $$(l "3")
    }

aeson :: Package.Dependency
aeson =
  Package.Dependency
    { name = "aeson",
      minVersion = $$(l "2"),
      maxVersion = $$(l "3")
    }

modelieroBase :: Package.Dependency
modelieroBase =
  Package.Dependency
    { name = "modeliero-base",
      minVersion = $$(l "1"),
      maxVersion = $$(l "1.1")
    }
