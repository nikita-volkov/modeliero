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
      Int
      -- | Max value.
      Int
  | IntegerType
      -- | Min value.
      (Maybe Integer)
      -- | Max value.
      (Maybe Integer)
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
            ${params.name} <$> ${genQualifier}chooseInt (${min}, ${max})
          |],
          [j|
            const []
          |]
        )
    IntegerType min max ->
      pure case min of
        Just min -> case max of
          Just max ->
            ( [j|
                ${params.name} <$> ${genQualifier}chooseInteger (${min}, ${max})
              |],
              [j|
                const []
              |]
            )
          Nothing ->
            ( [j|
                ${params.name} <$> suchThat (${genQualifier}chooseAny) (\x -> x >= ${min})
              |],
              [j|
                const []
              |]
            )
        Nothing -> case max of
          Just max ->
            ( [j|
                ${params.name} <$> suchThat (${genQualifier}chooseAny) (\x -> x <= ${max})
              |],
              [j|
                const []
              |]
            )
          Nothing ->
            ( [j|
                ${params.name} <$> ${genQualifier}chooseAny
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
              length <- ${genQualifier}chooseInt (${minLength}, ${maxLength})
              string <- ${genQualifier}vectorOf length ${arbitraryQualifier}arbitrary
              pure (${params.name} (fromString string))
          |],
          [j|
            \(${params.name} text) -> do
              toTake <- enumFromTo ${minLength} (${textQualifier}length text)
              pure (${params.name} (${textQualifier}take toTake text))
          |]
        )
  pure
    [j|
      instance ${arbitraryQualifier}Arbitrary ${params.name} where
        arbitrary = ${arbitraryBody}
        shrink = ${shrinkBody}
    |]
