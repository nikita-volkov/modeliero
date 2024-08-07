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

modelieroBaseSpecial :: Import
modelieroBaseSpecial =
  Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase.Classes.Special"
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
