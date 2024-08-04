{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Templates.RefinedArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Namespace qualified as Namespace
import Modeliero.Dsls.Package qualified as Package
import Modeliero.Imports qualified as Imports

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
      evalCont do
        withBodies
          <$> cont (Code.splicing arbitraryBody)
          <*> cont (Code.splicing shrinkBody)
      where
        withBodies arbitraryBody shrinkBody =
          Code.textBlock
            [j|
              instance ${arbitraryQualifier}Arbitrary ${params.name} where
                arbitrary = $arbitraryBody
                shrink = $shrinkBody
            |]
        (arbitraryBody, shrinkBody) = case params.type_ of
          IntType min max ->
            ( Code.textBlock
                [j|
                ${params.name} <$$> ${genQualifier}choose ($min, $max)
              |],
              Code.textBlock
                [j|
                  const []
                |]
            )
          TextType minLength maxLength ->
            ( Code.textBlock
                [j|
                  do
                    length <- ${genQualifier}chooseInt ($minLength, $maxLength)
                    string <- ${genQualifier}vectorOf length ${arbitraryQualifier}arbitrary
                    pure (${params.name} (fromString string))
                |],
              Code.importing Imports.text \textQualifier ->
                Code.textBlock
                  [j|
                    \(${params.name} text) -> do
                      toTake <- enumFromTo $minLength (${textQualifier}length text)
                      pure (${params.name} (${textQualifier}take toTake text))
                  |]
            )
