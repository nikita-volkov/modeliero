-- | Model of the parameters.
module Modeliero.Codegens.Haskell.Params where

import Coalmine.Prelude

data Model = Model
  { name :: Slug,
    version :: NonEmpty Word,
    types :: [TypeDeclaration],
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
  | RefinedTypeDefinition Refinement
  | SumTypeDefinition [Variant]

-- *** Product

data Field = Field
  { name :: Slug,
    -- | An override for the automatically generated name from the slug.
    -- When it\'s empty the automatically generated value will be used.
    jsonName :: Text,
    docs :: Text,
    type_ :: ValueType
  }

data ValueType
  = PlainValueType PlainType
  | MaybeValueType PlainType
  | VectorValueType PlainType
  | HashMapValueType PlainType PlainType

data PlainType
  = -- | Reference to a type defined in this model.
    LocalPlainType Slug
  | -- | One of the finite set of standard types with extra support.
    StandardPlainType StandardType
  | -- | Any type from any package with a limited support.
    CustomPlainType CustomType

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

-- *** Refinement

data Refinement
  = TextRefinement TextRestrictions
  | IntegerRefinement IntegerRestrictions
  | DoubleRefinement DoubleRestrictions

data TextRestrictions = TextRestrictions
  { minLength :: Int,
    maxLength :: Int,
    charsetRangeList :: Maybe [(Char, Char)],
    regexp :: Maybe Text
  }

data IntegerRestrictions = IntegerRestrictions
  { min :: Integer,
    max :: Integer
  }

data DoubleRestrictions = DoubleRestrictions
  { min :: Double,
    max :: Double
  }

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

data Aeson = Aeson
  { casing :: Casing
  }

data Casing
  = CamelCasing
  | SnakeCasing
  | KebabCasing
