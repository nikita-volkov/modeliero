{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Templates.RefinedArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Namespace qualified as Namespace
import Modeliero.Dsls.Package qualified as Package
import Modeliero.Imports qualified as Imports
import StructureKit.Charset qualified as Charset

data Params = Params
  { name :: Text,
    type_ :: Type
  }

type Result = Code.Code

data Type
  = IntType
      -- | Min value.
      Integer
      -- | Max value.
      Integer
  | TextType
      -- | Min length.
      Int
      -- | Max length.
      Int

compile :: Params -> Result
compile params =
  evalCont do
    withImports
      <$> cont (Code.importing Imports.quickCheckArbitrary)
      <*> cont (Code.importing Imports.quickCheckGen)
  where
    withImports arbitraryQualifier genQualifier =
      Code.textBlock
        [j|
          instance ${arbitraryQualifier}Arbitrary ${params.name} where
            arbitrary = $arbitraryBody
        |]
      where
        arbitraryBody = case params.type_ of
          IntType min max ->
            [j|
              ${params.name} <$$> ${genQualifier}choose ($min, $max)
            |]
          TextType minLength maxLength ->
            [j|
              do
                length <- ${genQualifier}chooseInt ($minLength, $maxLength)
                string <- ${genQualifier}vectorOf length ${arbitraryQualifier}arbitrary
                pure (${params.name} (fromString string))
            |]
