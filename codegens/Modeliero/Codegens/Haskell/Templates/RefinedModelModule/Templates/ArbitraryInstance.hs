module Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.ArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

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
  arbitraryQualifier <- requestImport Imports.quickCheckArbitrary
  genQualifier <- requestImport Imports.quickCheckGen
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
      textQualifier <- requestImport Imports.text
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
