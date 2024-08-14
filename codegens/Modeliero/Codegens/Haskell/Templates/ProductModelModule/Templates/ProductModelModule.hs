module Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.ProductModelModule where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.InModule
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.ArbitraryInstance qualified as Templates.ProductArbitraryInstance
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.DataTypeDeclaration qualified as Templates.ProductDataTypeDeclaration
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.FromJsonInstance qualified as Templates.FromJsonInstance
import Modeliero.Codegens.Haskell.Templates.ProductModelModule.Templates.ToJsonInstance qualified as Templates.ToJsonInstance

data Params = Params
  { name :: Text,
    haddock :: Text,
    fields :: [Field],
    instances :: Instances
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
  { show :: Bool,
    eq :: Bool,
    ord :: Bool,
    generic :: Bool,
    aeson :: Bool,
    arbitrary :: Bool
  }

compile :: Params -> Result
compile params = do
  registerExport [i|${params.name}(..)|]
  decls <-
    (sequence . catMaybes)
      [ Just
          $ Templates.ProductDataTypeDeclaration.compile
            Templates.ProductDataTypeDeclaration.Params
              { name = params.name & to,
                haddock = params.haddock,
                fields =
                  params.fields
                    & fmap
                      ( \field ->
                          Templates.ProductDataTypeDeclaration.Field
                            { name = field.name & to,
                              type_ = field.type_ & to,
                              haddock = field.haddock
                            }
                      ),
                derivings =
                  Templates.ProductDataTypeDeclaration.Derivings
                    { show = params.instances.show,
                      eq = params.instances.eq,
                      ord = params.instances.ord,
                      generic = params.instances.generic
                    }
              },
        if params.instances.aeson
          then
            Just
              $ Templates.ToJsonInstance.compile
                Templates.ToJsonInstance.Params
                  { typeName = params.name,
                    fields =
                      params.fields
                        & fmap
                          ( \field ->
                              Templates.ToJsonInstance.Field
                                { haskellName = field.name,
                                  jsonName = field.jsonName
                                }
                          )
                  }
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
                        & fmap
                          (\field -> field.name)
                  }
          else Nothing
      ]
  pure (TextBlock.intercalate "\n\n" decls)
