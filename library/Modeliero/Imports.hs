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
