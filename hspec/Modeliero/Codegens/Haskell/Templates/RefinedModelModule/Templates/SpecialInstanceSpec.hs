module Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.SpecialInstanceSpec where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.ExtrasFor.Hspec
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.SpecialInstance qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "Cases" do
    describe "Month" do
      let params =
            Subject.Params
              { typeName = "Month",
                type_ = Subject.IntType 1 12,
                specialClassImport = Imports.special
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V where
            
            import Data.Text qualified as Text
            import ModelieroBase.Classes.Special qualified as Special
            
            instance Special.Special Month where
              type GeneralizationOf Month = Int
              type SpecializationError Month = Text.Text
              specialize value = do
                when (value < 1) $
                  Left ("Value is smaller than 1: " <> fromString (show value))
                when (value > 12) $
                  Left ("Value is larger than 12: " <> fromString (show value))
                pure (Month value)
              generalize (Month base) = base
          |]

    describe "Phone" do
      let params =
            Subject.Params
              { typeName = "Phone",
                type_ = Subject.TextType 5 10,
                specialClassImport = Imports.special
              }

      compilingProducesExpectedContent Subject.compile params
        $ [i|
            module Z.V where
            
            import Data.Text qualified as Text
            import ModelieroBase.Classes.Special qualified as Special
            
            instance Special.Special Phone where
              type GeneralizationOf Phone = Text.Text
              type SpecializationError Phone = Text.Text
              specialize value = do
                let length = Text.length value
                when (length < 5) $
                  Left ("Length is smaller than 5: " <> fromString (show length))
                when (length > 10) $
                  Left ("Length is larger than 10: " <> fromString (show length))
                pure (Phone value)
              generalize (Phone base) = base
          |]
