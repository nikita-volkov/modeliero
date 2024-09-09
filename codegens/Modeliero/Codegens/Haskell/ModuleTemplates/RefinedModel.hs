{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

module Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel
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
import Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.ArbitraryInstance qualified as Templates.ArbitraryInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.DataTypeDeclaration qualified as Templates.DataTypeDeclaration
import Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.FromJsonInstance qualified as Templates.FromJsonInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.SpecialInstance qualified as Templates.SpecialInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.RefinedModel.Templates.ToJsonInstance qualified as Templates.ToJsonInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.TextModel qualified as TextModelModule
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.SnippetTemplates qualified as SnippetTemplates

data Params = Params
  { name :: Slug,
    docs :: Text,
    anonymizable :: Bool,
    refinement :: Params.Refinement,
    instances :: Params.Instances
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params = do
  registerExport (Slug.toUpperCamelCaseText params.name)

  case params.refinement of
    Params.TextRefinement restrictions -> do
      TextModelModule.Params
        { name = params.name,
          docs = params.docs,
          minLength = restrictions.minLength & Just,
          maxLength = restrictions.maxLength & Just,
          anonymizable = params.anonymizable,
          instances = params.instances
        }
        & TextModelModule.compile
    Params.IntegerRefinement _ ->
      [ Just do
          baseType <-
            -- TODO: Add analysis on the bounds to determine whether it should be Integer or Int.
            requestImport Imports.basePreludeRoot <&> \qfr -> qfr <> "Int"
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
                      generic = False,
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
          -- TODO: Add analysis on the bounds to determine whether it should be Integer or Int.
          Templates.FromJsonInstance.compile
            Templates.FromJsonInstance.Params
              { name = params.name & Slug.toUpperCamelCaseText,
                type_ = Templates.FromJsonInstance.IntType
              }
      ]
        & catMaybes
        & sequence
        & liftDeclarations

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
