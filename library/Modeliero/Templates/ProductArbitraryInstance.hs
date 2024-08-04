module Modeliero.Templates.ProductArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Dsls.InModule
import Modeliero.Imports qualified as Imports

data Params = Params
  { name :: Text,
    fields :: [Text]
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  arbitraryQualifier <- import_ Imports.quickCheckArbitrary
  let arbitraryAssignments =
        params.fields
          & foldMap
            ( \fieldName ->
                [j|
                  $fieldName <- ${arbitraryQualifier}arbitrary
                
                |]
            )
      shrinkAssignments =
        params.fields
          & foldMap
            ( \fieldName ->
                [j|
                  $fieldName <- ${arbitraryQualifier}shrink value.$fieldName
                
                |]
            )
  pure
    [j|
      instance ${arbitraryQualifier}Arbitrary ${params.name} where
        arbitrary ${params.name}{..} = do
          ${arbitraryAssignments}pure ${params.name}{..}
        shrink value = do
          ${shrinkAssignments}pure ${params.name}{..}
    |]
