module Modeliero.Codegens.Haskell.Dependencies where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.Package (Dependency (..))

base :: Dependency
base =
  Dependency
    { name = "base",
      minVersion = $$(l "4.14"),
      maxVersion = $$(l "5")
    }

quickCheck :: Dependency
quickCheck =
  Dependency
    { name = "QuickCheck",
      minVersion = $$(l "2.15"),
      maxVersion = $$(l "3")
    }

text :: Dependency
text =
  Dependency
    { name = "text",
      minVersion = $$(l "2"),
      maxVersion = $$(l "3")
    }

aeson :: Dependency
aeson =
  Dependency
    { name = "aeson",
      minVersion = $$(l "2"),
      maxVersion = $$(l "3")
    }

modelieroBase :: Dependency
modelieroBase =
  Dependency
    { name = "modeliero-base",
      minVersion = $$(l "1"),
      maxVersion = $$(l "1.1")
    }