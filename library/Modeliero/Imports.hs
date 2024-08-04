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
