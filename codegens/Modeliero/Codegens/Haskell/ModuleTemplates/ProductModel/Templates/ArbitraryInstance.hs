module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.Templates.ArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports

data Params = Params
  { name :: Text,
    fields :: [Text]
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  arbitraryQualifier <- requestImport Imports.quickCheckArbitrary
  let arbitraryAssignments =
        params.fields
          & foldMap
            ( \fieldName ->
                [j|
                  ${fieldName} <- ${arbitraryQualifier}arbitrary
                
                |]
            )
      shrinkAssignments =
        params.fields
          & foldMap
            ( \fieldName ->
                [j|
                  ${fieldName} <- ${arbitraryQualifier}shrink value.${fieldName}
                
                |]
            )
  pure
    [j|
      instance ${arbitraryQualifier}Arbitrary ${params.name} where
        arbitrary = do
          ${arbitraryAssignments}pure ${params.name}{..}
        shrink value = do
          ${shrinkAssignments}pure ${params.name}{..}
    |]
