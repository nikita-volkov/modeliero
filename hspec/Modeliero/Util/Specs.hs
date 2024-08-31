module Modeliero.Util.Specs where

import Coalmine.Prelude
import Test.Hspec

itFailsWithInput :: (ToJSON a) => String -> a -> Spec
itFailsWithInput label =
  it label . expectationFailure . toList . renderAsYamlText
