module Modeliero.Preludes.Spec
  ( module Modeliero.Preludes.Spec,
    module Test.Hspec,
    module Test.Hspec.Expectations.Contrib,
    module Coalmine.Prelude,
  )
where

import Coalmine.Prelude hiding (Arg)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

itFailsWithInput :: (ToJSON a) => String -> a -> Spec
itFailsWithInput label =
  it label . expectationFailure . toList . renderAsYamlText
