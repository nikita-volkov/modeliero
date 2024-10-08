module Modeliero.Codegens.Haskell.Imports where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dependencies qualified as Dependencies
import Modeliero.Codegens.Haskell.Dsls.Code (Import (..))
import Modeliero.Codegens.Haskell.Dsls.Package (Dependency)

external :: Dependency -> Text -> Import
external dependency name =
  Import
    { dependency = Just dependency,
      name
    }

fromQuickCheck :: Text -> Import
fromQuickCheck name =
  Import
    { dependency = Just Dependencies.quickCheck,
      name
    }

quickCheckRoot :: Import
quickCheckRoot = fromQuickCheck "Test.QuickCheck"

quickCheckArbitrary :: Import
quickCheckArbitrary = fromQuickCheck "Test.QuickCheck.Arbitrary"

quickCheckGen :: Import
quickCheckGen = fromQuickCheck "Test.QuickCheck.Gen"

fromText :: Text -> Import
fromText name =
  Import
    { dependency = Just Dependencies.text,
      name
    }

textRoot :: Import
textRoot = fromText "Data.Text"

textLazy :: Import
textLazy = fromText "Data.Text.Lazy"

fromAeson :: Text -> Import
fromAeson name =
  Import
    { dependency = Just Dependencies.aeson,
      name
    }

aeson :: Import
aeson = fromAeson "Data.Aeson"

aesonKeyMap :: Import
aesonKeyMap = fromAeson "Data.Aeson.KeyMap"

aesonKey :: Import
aesonKey = fromAeson "Data.Aeson.Key"

aesonTypes :: Import
aesonTypes = fromAeson "Data.Aeson.Types"

special :: Import
special = modelieroBaseRoot

anonymizable :: Import
anonymizable = modelieroBaseRoot

modelieroBaseRoot :: Import
modelieroBaseRoot =
  Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase"
    }

fromBase :: Text -> Import
fromBase name =
  Import
    { dependency = Just Dependencies.base,
      name = name
    }

basePrelude :: Import
basePrelude = fromBase "Prelude"

baseGenerics :: Import
baseGenerics = fromBase "GHC.Generics"

scientific :: Import
scientific =
  Import
    { dependency = Just Dependencies.scientific,
      name = "Data.Scientific"
    }

hashable :: Import
hashable =
  Import
    { dependency = Just Dependencies.hashable,
      name = "Data.Hashable"
    }

iproute :: Import
iproute =
  Import
    { dependency = Just Dependencies.iproute,
      name = "Data.IP"
    }

isomorphismClass :: Import
isomorphismClass =
  Import
    { dependency = Just Dependencies.isomorphismClass,
      name = "IsomorphismClass"
    }

dataCoerce :: Import
dataCoerce =
  Import
    { dependency = Just Dependencies.base,
      name = "Data.Coerce"
    }

basePreludeRoot :: Import
basePreludeRoot =
  Import
    { dependency = Just Dependencies.basePrelude,
      name = "BasePrelude"
    }

uuidRoot :: Import
uuidRoot =
  Import
    { dependency = Just Dependencies.basePrelude,
      name = "Data.UUID"
    }

attoparsecText :: Import
attoparsecText =
  Import
    { dependency = Just Dependencies.attoparsec,
      name = "Data.Attoparsec.Text"
    }

vectorRoot :: Import
vectorRoot = external Dependencies.vector "Data.Vector"
