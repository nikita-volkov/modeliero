-- | Refined text model.
module Modeliero.Codegens.Haskell.ModuleTemplates.TextModel
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.Prelude
import Coalmine.Slug qualified as Slug
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.Params qualified as Params
import Modeliero.Codegens.Haskell.SnippetTemplates

data Params = Params
  { name :: Slug,
    docs :: Text,
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    instances :: Params.Instances,
    anonymizable :: Bool
  }

type Result = InModule TextBlock

compile :: Params -> Result
compile params =
  [ compileDataTypeDeclaration,
    compileLiteralInstance,
    compileArbitraryInstance,
    compileRefinedTextAnonymizableInstance
  ]
    & traverse ($ params)
    & liftDeclarations

compileDataTypeDeclaration :: Params -> InModule TextBlock
compileDataTypeDeclaration params = do
  textQfr <- requestImport Imports.textRoot
  basePreludeQfr <- requestImport Imports.basePreludeRoot
  hashableQfr <- requestImport Imports.hashable
  modelieroBaseQfr <- requestImport Imports.modelieroBaseRoot
  aesonQfr <- requestImport Imports.aeson
  DataType
    { name = params.name & Slug.toUpperCamelCaseText & to,
      haddock = params.docs,
      baseTypeSig = to (textQfr <> "Text"),
      stockDerivings = [],
      newtypeDerivings =
        [ basePreludeQfr <> "Show",
          basePreludeQfr <> "Read",
          basePreludeQfr <> "IsString",
          basePreludeQfr <> "Eq",
          basePreludeQfr <> "Ord",
          hashableQfr <> "Hashable",
          aesonQfr <> "ToJSON",
          aesonQfr <> "ToJSONKey"
        ]
          & fmap to,
      derivingVia =
        Just
          DerivingVia
            { derivings =
                [ aesonQfr <> "FromJSON",
                  aesonQfr <> "FromJSONKey"
                ]
                  & fmap to
                  & ParensList,
              viaSig =
                [ modelieroBaseQfr,
                  "ViaLiteral ",
                  params.name
                    & Slug.toUpperCamelCaseText
                ]
                  & mconcat
                  & to
            }
    }
    & toBroadBuilder
    & pure

compileLiteralInstance :: Params -> InModule TextBlock
compileLiteralInstance params = do
  textQfr <- requestImport Imports.textRoot
  basePreludeQfr <- requestImport Imports.basePreludeRoot
  literalQfr <- requestImport Imports.modelieroBaseRoot
  attoparsecQfr <- requestImport Imports.attoparsecText
  RefinedTextLiteralInstance
    { name = params.name & Slug.toUpperCamelCaseText & to,
      minLength = params.minLength,
      maxLength = params.maxLength,
      ..
    }
    & toBroadBuilder
    & pure

compileArbitraryInstance :: Params -> InModule TextBlock
compileArbitraryInstance params = do
  quickCheckQfr <- requestImport Imports.quickCheckRoot
  RefinedTextArbitraryInstance
    { name = params.name & Slug.toUpperCamelCaseText & to,
      minLength = params.minLength & fromMaybe 0,
      maxLength = params.maxLength & fromMaybe 10000,
      ..
    }
    & toBroadBuilder
    & pure

compileRefinedTextAnonymizableInstance :: Params -> InModule TextBlock
compileRefinedTextAnonymizableInstance params = do
  textQfr <- requestImport Imports.textRoot
  modelieroBaseQfr <- requestImport Imports.modelieroBaseRoot
  RefinedTextAnonymizableInstance
    { name = params.name & Slug.toUpperCamelCaseText & to,
      minLength = params.minLength & fromMaybe 0,
      maxLength = params.maxLength & fromMaybe 10000,
      anonymizable = params.anonymizable,
      ..
    }
    & toBroadBuilder
    & pure
