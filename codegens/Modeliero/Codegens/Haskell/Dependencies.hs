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

basePrelude :: Dependency
basePrelude =
  Dependency
    { name = "base-prelude",
      minVersion = $$(l "1.6.1.1"),
      maxVersion = $$(l "1.7")
    }

uuid :: Dependency
uuid =
  Dependency
    { name = "uuid",
      minVersion = $$(l "1.3"),
      maxVersion = $$(l "1.4")
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
      minVersion = $$(l "0"),
      maxVersion = $$(l "0.1")
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

isomorphismClass :: Dependency
isomorphismClass =
  Dependency
    { name = "isomorphism-class",
      minVersion = $$(l "0.1.0.12"),
      maxVersion = $$(l "0.2")
    }

attoparsec :: Dependency
attoparsec =
  Dependency
    { name = "attoparsec",
      minVersion = $$(l "0.12"),
      maxVersion = $$(l "0.15")
    }

vector :: Dependency
vector =
  Dependency
    { name = "vector",
      minVersion = $$(l "0.13"),
      maxVersion = $$(l "0.14")
    }
