module Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.Templates.ProductModelModule where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Imports qualified as Imports
import Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.SnippetTemplates qualified as SnippetTemplates
import Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.Templates.ArbitraryInstance qualified as Templates.ProductArbitraryInstance
import Modeliero.Codegens.Haskell.ModuleTemplates.ProductModel.Templates.FromJsonInstance qualified as Templates.FromJsonInstance
import Modeliero.Codegens.Haskell.SnippetTemplates qualified as SnippetTemplates

data Params = Params
  { name :: Text,
    haddock :: Text,
    fields :: [Field],
    instances :: Instances,
    anonymizable :: Bool
  }

type Result = InModule TextBlock

data Field = Field
  { name :: Text,
    type_ :: Text,
    haddock :: Text,
    jsonName :: Text,
    nullable :: Bool
  }

data Instances = Instances
  { aeson :: Bool,
    arbitrary :: Bool,
    anonymizable :: Bool,
    hashable :: Bool
  }

compile :: Params -> Result
compile params = do
  registerExport [i|${params.name}(..)|]
  decls <-
    (sequence . catMaybes)
      [ Just do
          basePreludeQfr <- requestImport Imports.basePreludeRoot
          SnippetTemplates.DataTypeDeclaration
            { name = params.name & to,
              haddock = params.haddock,
              fields =
                params.fields
                  & fmap
                    ( \field ->
                        SnippetTemplates.DataTypeDeclarationField
                          { name = field.name & to,
                            -- TODO: Implement Maybe
                            type_ = field.type_ & to,
                            haddock = field.haddock
                          }
                    ),
              stockDerivings =
                ["Show", "Read", "Eq", "Ord"]
                  & fmap (to basePreludeQfr <>)
            }
            & toBroadBuilder
            & pure,
        if params.instances.hashable
          then Just do
            hashableQfr <- to <$> requestImport Imports.hashable
            SnippetTemplates.HashableInstance
              { hashableQfr,
                typeName = params.name & to,
                fieldAccessors =
                  params.fields
                    & fmap (.name)
                    & fmap to
              }
              & toBroadBuilder
              & pure
          else Nothing,
        if params.instances.aeson
          then Just do
            aesonQfr <- to <$> requestImport Imports.aeson
            aesonKeyMapQfr <- to <$> requestImport Imports.aesonKeyMap
            SnippetTemplates.ToJsonInstance
              { typeName = params.name & to,
                aesonQfr,
                aesonKeyMapQfr,
                fields =
                  params.fields
                    & fmap
                      ( \field ->
                          SnippetTemplates.ToJsonInstanceFieldExp
                            { required = not field.nullable,
                              key = field.jsonName,
                              valueExp = [j|value.${field.name}|],
                              aesonQfr
                            }
                      )
                    & SnippetTemplates.CommaSeparated
              }
              & toBroadBuilder
              & pure
          else Nothing,
        if params.instances.aeson
          then
            Just
              $ Templates.FromJsonInstance.compile
                Templates.FromJsonInstance.Params
                  { typeName = params.name,
                    fields =
                      params.fields
                        & fmap
                          ( \field ->
                              Templates.FromJsonInstance.Field
                                { haskellName = field.name,
                                  jsonName = field.jsonName,
                                  nullable = field.nullable
                                }
                          )
                  }
          else Nothing,
        if params.instances.arbitrary
          then
            Just
              $ Templates.ProductArbitraryInstance.compile
                Templates.ProductArbitraryInstance.Params
                  { name = params.name,
                    fields =
                      params.fields
                        & fmap (\field -> field.name)
                  }
          else Nothing,
        if params.instances.anonymizable
          then Just do
            modelieroBaseQfr <- requestImport Imports.modelieroBaseRoot
            SnippetTemplates.ProductAnonymizableInstance
              { name = params.name & to,
                fieldNames =
                  params.fields
                    & fmap (\field -> field.name & to),
                anonymizable = params.anonymizable,
                modelieroBaseQfr
              }
              & toBroadBuilder
              & pure
          else Nothing
      ]
  pure (TextBlock.intercalate "\n\n" decls)
