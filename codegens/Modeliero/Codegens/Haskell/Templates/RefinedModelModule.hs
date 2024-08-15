{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Codegens.Haskell.Templates.RefinedModelModule
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.ArbitraryInstance qualified as Templates.ArbitraryInstance
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.FromJsonInstance qualified as Templates.FromJsonInstance
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.SpecialInstance qualified as Templates.SpecialInstance
import Modeliero.Codegens.Haskell.Templates.RefinedModelModule.Templates.ToJsonInstance qualified as Templates.ToJsonInstance

data Params = Params
  { name :: Slug,
    docs :: Text,
    refinement :: Params.Refinement,
    instances :: Params.Instances
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name)
  decls <-
    (sequence . catMaybes)
      [ Just do
          baseType <- case params.refinement of
            Params.TextRefinement _ ->
              requestImport Imports.text <&> \qfr -> qfr <> "Text"
            Params.IntegerRefinement _ ->
              -- TODO: Add analysis on the bounds to determine whether it should be Integer or Int.
              requestImport Imports.basePrelude <&> \qfr -> qfr <> "Int"
          Templates.DataTypeDeclaration.compile
            Templates.DataTypeDeclaration.Params
              { name = params.name & Slug.toUpperCamelCaseText & to,
                haddock = params.docs,
                baseType = baseType & to,
                derivings =
                  Templates.DataTypeDeclaration.Derivings
                    { show = params.instances.show,
                      eq = params.instances.eq,
                      ord = params.instances.ord,
                      generic = params.instances.generic,
                      hashable = params.instances.hashable
                    }
              },
        Just (compileSpecial params),
        compileArbitrary params,
        params.instances.aeson <&> \_ ->
          Templates.ToJsonInstance.compile
            Templates.ToJsonInstance.Params
              { name = params.name & Slug.toUpperCamelCaseText
              },
        params.instances.aeson <&> \_ ->
          Templates.FromJsonInstance.compile
            Templates.FromJsonInstance.Params
              { name = params.name & Slug.toUpperCamelCaseText,
                type_ = case params.refinement of
                  Params.IntegerRefinement _ ->
                    -- TODO: Add analysis on the bounds to determine whether it should be Integer or Int.
                    Templates.FromJsonInstance.IntType
                  _ ->
                    error "TODO"
              }
      ]
  pure (TextBlock.intercalate "\n\n" decls)

compileArbitrary :: Params -> Maybe (InModule TextBlock)
compileArbitrary params =
  if params.instances.arbitrary
    then
      Just
        $ Templates.ArbitraryInstance.compile
          Templates.ArbitraryInstance.Params
            { name =
                params.name & Slug.toUpperCamelCaseText,
              type_ =
                case params.refinement of
                  Params.TextRefinement textRestrictions ->
                    Templates.ArbitraryInstance.TextType
                      textRestrictions.minLength
                      textRestrictions.maxLength
                  Params.IntegerRefinement integerRestrictions ->
                    if integerRestrictions.min >= toInteger @Int minBound && integerRestrictions.max <= toInteger @Int maxBound
                      then
                        Templates.ArbitraryInstance.IntType
                          (fromIntegral integerRestrictions.min)
                          (fromIntegral integerRestrictions.max)
                      else
                        Templates.ArbitraryInstance.IntegerType
                          (Just integerRestrictions.min)
                          (Just integerRestrictions.max)
            }
    else Nothing

compileSpecial :: Params -> InModule TextBlock
compileSpecial params =
  Templates.SpecialInstance.compile
    Templates.SpecialInstance.Params
      { typeName = params.name & Slug.toUpperCamelCaseText,
        type_ =
          case params.refinement of
            Params.TextRefinement textRestrictions ->
              Templates.SpecialInstance.TextType textRestrictions.minLength textRestrictions.maxLength
            Params.IntegerRefinement integerRestrictions ->
              Templates.SpecialInstance.IntType integerRestrictions.min integerRestrictions.max,
        specialClassImport = Imports.special
      }
