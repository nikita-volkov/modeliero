module Modeliero.Codegens.Haskell.Imports where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dependencies qualified as Dependencies
import Modeliero.Codegens.Haskell.Dsls.Code (Import (..))

fromQuickCheck :: Text -> Import
fromQuickCheck name =
  Import
    { dependency = Just Dependencies.quickCheck,
      name
    }

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

text :: Import
text = fromText "Data.Text"

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
special =
  Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase.Classes.Special"
    }

anonymizable :: Import
anonymizable =
  Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase.Classes.Anonymizable"
    }

modelieroBaseTypes :: Import
modelieroBaseTypes =
  Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase.Types"
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
