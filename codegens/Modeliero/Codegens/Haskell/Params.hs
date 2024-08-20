-- | Model of the parameters.
module Modeliero.Codegens.Haskell.Params where

import Coalmine.Prelude

data Model = Model
  { name :: Slug,
    version :: NonEmpty Word,
    types :: [TypeDeclaration],
    instances :: Instances
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- ** TypeDeclaration

data TypeDeclaration = TypeDeclaration
  { name :: Slug,
    docs :: Text,
    definition :: TypeDefinition
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data TypeDefinition
  = ProductTypeDefinition [Field]
  | RefinedTypeDefinition Refinement
  | SumTypeDefinition [Variant]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- *** Product

data Field = Field
  { name :: Slug,
    -- | An override for the automatically generated name from the slug.
    -- When it\'s empty the automatically generated value will be used.
    jsonName :: Text,
    docs :: Text,
    type_ :: ValueType
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ValueType
  = PlainValueType PlainType
  | MaybeValueType PlainType
  | VectorValueType PlainType
  | HashMapValueType PlainType PlainType
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data PlainType
  = -- | Reference to a type defined in this model.
    LocalPlainType Slug
  | -- | One of the finite set of standard types with extra support.
    StandardPlainType StandardType
  | -- | Any type from any package with a limited support.
    CustomPlainType CustomType
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data CustomType = CustomType
  { packageName :: Text,
    minVersion :: NonEmpty Word,
    maxVersion :: NonEmpty Word,
    module_ :: Text,
    name :: Text
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- *** Sum

data Variant
  = Variant
  { name :: Slug,
    -- | An override for the automatically generated name from the slug.
    -- When it\'s empty the automatically generated value will be used.
    jsonName :: Text,
    type_ :: ValueType,
    docs :: Text,
    anonymizable :: Bool
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- *** Refinement

data Refinement
  = TextRefinement TextRestrictions
  | IntegerRefinement IntegerRestrictions
  | DoubleRefinement DoubleRestrictions
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data TextRestrictions = TextRestrictions
  { minLength :: Int,
    maxLength :: Int,
    charsetRangeList :: Maybe [(Char, Char)],
    regexp :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data IntegerRestrictions = IntegerRestrictions
  { min :: Integer,
    max :: Integer
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data DoubleRestrictions = DoubleRestrictions
  { min :: Double,
    max :: Double
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- ** Instances

data Instances = Instances
  { show :: Bool,
    eq :: Bool,
    ord :: Bool,
    hashable :: Bool,
    generic :: Bool,
    aeson :: Maybe Aeson,
    arbitrary :: Bool,
    anonymizable :: Bool
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Aeson = Aeson
  { casing :: Casing
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Casing
  = CamelCasing
  | SnakeCasing
  | KebabCasing
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
