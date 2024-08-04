module Modeliero.Templates.RefinedArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Dsls.InModule
import Modeliero.Imports qualified as Imports

data Params = Params
  { name :: Text,
    type_ :: Type
  }

type Result = InModule TextBlock

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
compile params = do
  arbitraryQualifier <- import_ Imports.quickCheckArbitrary
  genQualifier <- import_ Imports.quickCheckGen
  (arbitraryBody, shrinkBody) <- case params.type_ of
    IntType min max ->
      pure
        ( [j|
            ${params.name} <$$> ${genQualifier}choose ($min, $max)
          |],
          [j|
            const []
          |]
        )
    TextType minLength maxLength -> do
      textQualifier <- import_ Imports.text
      pure
        ( [j|
            do
              length <- ${genQualifier}chooseInt ($minLength, $maxLength)
              string <- ${genQualifier}vectorOf length ${arbitraryQualifier}arbitrary
              pure (${params.name} (fromString string))
          |],
          [j|
            \(${params.name} text) -> do
              toTake <- enumFromTo $minLength (${textQualifier}length text)
              pure (${params.name} (${textQualifier}take toTake text))
          |]
        )
  pure
    [j|
      instance ${arbitraryQualifier}Arbitrary ${params.name} where
        arbitrary = $arbitraryBody
        shrink = $shrinkBody
    |]
