module Modeliero.Imports where

import Coalmine.Prelude
import Modeliero.Dependencies qualified as Dependencies
import Modeliero.Dsls.Code qualified as Code

quickCheckArbitrary :: Code.Import
quickCheckArbitrary =
  Code.Import
    { dependency = Just Dependencies.quickCheck,
      name = "Test.QuickCheck.Arbitrary"
    }

quickCheckGen :: Code.Import
quickCheckGen =
  Code.Import
    { dependency = Just Dependencies.quickCheck,
      name = "Test.QuickCheck.Gen"
    }

text :: Code.Import
text =
  Code.Import
    { dependency = Just Dependencies.text,
      name = "Data.Text"
    }

textLazy :: Code.Import
textLazy =
  Code.Import
    { dependency = Just Dependencies.text,
      name = "Data.Text.Lazy"
    }

aeson :: Code.Import
aeson =
  Code.Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson"
    }

aesonKeyMap :: Code.Import
aesonKeyMap =
  Code.Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson.KeyMap"
    }

aesonKey :: Code.Import
aesonKey =
  Code.Import
    { dependency = Just Dependencies.aeson,
      name = "Data.Aeson.Key"
    }

modelieroBaseSpecial :: Code.Import
modelieroBaseSpecial =
  Code.Import
    { dependency = Just Dependencies.modelieroBase,
      name = "ModelieroBase.Classes.Special"
    }
