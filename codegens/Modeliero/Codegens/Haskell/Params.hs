-- | Model of the parameters.
module Modeliero.Codegens.Haskell.Params where

import Coalmine.Prelude

data Model = Model
  { types :: [TypeDeclaration],
    instances :: Instances
  }

-- ** TypeDeclaration

data TypeDeclaration = TypeDeclaration
  { name :: Slug,
    docs :: Text,
    definition :: TypeDefinition
  }

data TypeDefinition
  = ProductTypeDefinition [Field]

data Field = Field
  { name :: Slug,
    docs :: Text,
    type_ :: FieldType
  }

data FieldType
  = PlainFieldType PlainType
  | MaybeFieldType PlainType
  | VectorFieldType PlainType
  | HashMapFieldType PlainType PlainType

data PlainType
  = LocalPlainType Slug
  | StandardPlainType StandardType
  | CustomPlainType CustomType

data StandardType
  = BoolStandardType
  | IntStandardType
  | DoubleStandardType
  | ScientificStandardType
  | TextStandardType
  | UuidStandardType
  | DayStandardType
  | UtcTimeStandardType
  | LocalTimeStandardType
  | ZonedTimeStandardType
  | TimeZoneStandardType
  | EmailStandardType
  | UrlStandardType

data CustomType = CustomType
  { packageName :: Text,
    minVersion :: NonEmpty Word,
    maxVersion :: NonEmpty Word,
    module_ :: Text,
    name :: Text
  }

-- ** Instances

data Instances = Instances
  { show :: Bool,
    eq :: Bool,
    ord :: Bool,
    generic :: Bool,
    aeson :: Maybe Aeson,
    arbitrary :: Bool
  }

data Aeson = Aeson
  { casing :: Casing
  }

data Casing
  = CamelCasing
  | SnakeCasing
  | KebabCasing
