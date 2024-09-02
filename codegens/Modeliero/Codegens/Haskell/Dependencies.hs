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

scientific :: Dependency
scientific =
  Dependency
    { name = "scientific",
      minVersion = $$(l "0.3"),
      maxVersion = $$(l "0.4")
    }

hashable :: Dependency
hashable =
  Dependency
    { name = "hashable",
      minVersion = $$(l "1.4"),
      maxVersion = $$(l "2")
    }

iproute :: Dependency
iproute =
  Dependency
    { name = "iproute",
      minVersion = $$(l "1.7"),
      maxVersion = $$(l "1.8")
    }
