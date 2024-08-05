module Modeliero.Imports where

import Coalmine.Prelude
import Modeliero.Dependencies qualified as Dependencies
import Modeliero.Dsls.Code (Import (..))

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

aeson :: Import
aeson =
  Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson"
    }

aesonKeyMap :: Import
aesonKeyMap =
  Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson.KeyMap"
    }

aesonKey :: Import
aesonKey =
  Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson.Key"
    }

aesonTypes :: Import
aesonTypes =
  Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson.Types"
    }

modelieroBaseSpecial :: Import
modelieroBaseSpecial =
  Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase.Classes.Special"
    }
