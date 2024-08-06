module Modeliero.Codegens.Haskell.Imports where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dependencies qualified as Dependencies
import Modeliero.Codegens.Haskell.Dsls.Code (Import (..))

quickCheckArbitrary :: Import
quickCheckArbitrary =
  Import
    { dependency = Just Dependencies.quickCheck,
      name = "Test.QuickCheck.Arbitrary"
    }

quickCheckGen :: Import
quickCheckGen =
  Import
    { dependency = Just Dependencies.quickCheck,
      name = "Test.QuickCheck.Gen"
    }

text :: Import
text =
  Import
    { dependency = Just Dependencies.text,
      name = "Data.Text"
    }

textLazy :: Import
textLazy =
  Import
    { dependency = Just Dependencies.text,
      name = "Data.Text.Lazy"
    }

fromAeson :: Text -> Import
fromAeson name =
  Import
    { dependency = Just Dependencies.aeson,
      name = name
    }

aeson :: Import
aeson = fromBase "Data.Aeson"

aesonKeyMap :: Import
aesonKeyMap = fromBase "Data.Aeson.KeyMap"

aesonKey :: Import
aesonKey = fromBase "Data.Aeson.Key"

aesonTypes :: Import
aesonTypes = fromBase "Data.Aeson.Types"

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
